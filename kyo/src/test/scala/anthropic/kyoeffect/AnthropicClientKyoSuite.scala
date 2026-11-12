package anthropic.kyoeffect

import _root_.kyo.*
import anthropic.*
import anthropic.MockApi.{json, sse, Key}
import anthropic.kyoeffect.AnthropicClientKyo.{accumulate, textDeltas}
import scala.concurrent.Future

class AnthropicClientKyoSuite extends munit.FunSuite {

  def withClient[A](replies: Reply*)(
      f: (MockApi, AnthropicClientKyo) => A < (Async & Abort[Throwable] & Scope)
  ): Future[A] = {
    val program: A < (Async & Abort[Throwable] & Scope) =
      for {
        api    <- Scope.acquire(new MockApi(replies))
        client <- AnthropicClientKyo.init(ClientConfig.apiKey(Key).copy(baseUrl = api.url))
        a      <- f(api, client)
      } yield a
    import AllowUnsafe.embrace.danger
    Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(Scope.run(program)).map(_.toFuture))
  }

  def message(text: String, stop: String = "end_turn"): String =
    s"""{"id": "msg_1", "type": "message", "role": "assistant", "model": "claude-opus-5-5",
       |"content": [{"type": "text", "text": "$text"}], "stop_reason": "$stop", "stop_sequence": null,
       |"usage": {"input_tokens": 3, "output_tokens": 2}}""".stripMargin

  val events: String = sse(
    json("""{"type": "message_start", "message": {"id": "msg_s", "type": "message", "role": "assistant", "model": "m", "content": [], "stop_reason": null, "usage": {"input_tokens": 5}}}"""),
    json("""{"type": "content_block_start", "index": 0, "content_block": {"type": "text", "text": ""}}"""),
    json("""{"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "Bună "}}"""),
    json("""{"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "ziua"}}"""),
    json("""{"type": "content_block_stop", "index": 0}"""),
    json("""{"type": "message_delta", "delta": {"stop_reason": "end_turn"}, "usage": {"output_tokens": 4}}"""),
    json("""{"type": "message_stop"}""")
  )

  test("create and count tokens") {
    withClient(Reply.ok(message("salut")), Reply.ok("""{"input_tokens": 7}""")) { (_, c) =>
      for {
        m <- c.create(MessagesRequest("claude-opus-5-5", 64).user("hi"))
        n <- c.countTokens(MessagesRequest("claude-opus-5-5", 64).user("hi"))
      } yield {
        assertEquals(m.text, "salut")
        assertEquals(n, 7L)
      }
    }
  }

  test("a failed call is a typed failure, and a bad configuration fails the client") {
    val error = """{"type": "error", "error": {"type": "invalid_request_error", "message": "nope"}}"""
    withClient(Reply(400, error)) { (_, c) =>
      for {
        call <- Abort.run[AnthropicException](c.create(MessagesRequest("m", 64).user("hi")))
        bad  <- Abort.run[AnthropicException](AnthropicClientKyo.init(ClientConfig.apiKey(Key).copy(baseUrl = "not a url")))
      } yield {
        call match {
          case Result.Failure(e: AnthropicException.ApiError) =>
            assertEquals(e.status, Some(400))
            assertEquals(e.kind, "invalid_request_error")
          case other => fail(s"$other")
        }
        assert(bad.failure.exists(_.isInstanceOf[AnthropicException.ConfigError]), bad)
      }
    }
  }

  test("the layer provides a client") {
    withClient(Reply.ok(message("salut"))) { (api, _) =>
      val ask = Env.use[AnthropicClientKyo](_.create(MessagesRequest("m", 64).user("hi")))
      Memo.run(Env.runLayer(AnthropicClientKyo.layer(ClientConfig.apiKey(Key).copy(baseUrl = api.url)))(ask)).map { m =>
        assertEquals(m.text, "salut")
      }
    }
  }

  test("a stream emits events, its text, and accumulates the final message") {
    withClient(Reply.ok(events)) { (_, c) =>
      val req = MessagesRequest("m", 64).user("hi")
      for {
        all  <- c.stream(req).run
        text <- c.stream(req).textDeltas.run
        last <- c.stream(req).accumulate.run
      } yield {
        assertEquals(all.map(_.kind).last, "message_stop")
        assertEquals(text.mkString, "Bună ziua")
        assertEquals(last.last.text, "Bună ziua")
        assertEquals(last.last.usage.outputTokens, 4L)
        assertEquals(last.last.stopReason, Some(StopReason.EndTurn))
      }
    }
  }

  test("a stream left early releases its connection when its scope closes") {
    withClient(Reply.ok(events).copy(stall = scala.concurrent.duration.FiniteDuration(10, "s"))) { (_, c) =>
      Clock.stopwatch.map { watch =>
        for {
          first <- Scope.run(c.stream(MessagesRequest("m", 64).user("hi")).take(2).run)
          took  <- watch.elapsed
        } yield {
          assertEquals(first.map(_.kind).toList, List("message_start", "content_block_start"))
          assert(took < 3.seconds, took)
        }
      }
    }
  }

  test("an error event fails the stream") {
    val body = sse(
      json("""{"type": "message_start", "message": {"id": "m", "type": "message", "role": "assistant", "model": "x", "content": [], "stop_reason": null, "usage": {}}}"""),
      json("""{"type": "error", "error": {"type": "overloaded_error", "message": "Overloaded"}}""")
    )
    withClient(Reply.ok(body)) { (_, c) =>
      Abort.run[AnthropicException](c.stream(MessagesRequest("m", 64).user("hi")).discard).map {
        case Result.Failure(e: AnthropicException.ApiError) => assertEquals(e.kind, "overloaded_error")
        case other                                           => fail(s"$other")
      }
    }
  }

  test("interrupting a slow call ends it promptly") {
    withClient(Reply.ok(message("late")).copy(delay = scala.concurrent.duration.FiniteDuration(10, "s"))) { (_, c) =>
      Clock.stopwatch.map { watch =>
        for {
          result <- Abort.run[Timeout](Async.timeout(300.millis)(c.create(MessagesRequest("m", 64).user("hi"))))
          took   <- watch.elapsed
        } yield {
          assert(result.isFailure, result)
          assert(took < 3.seconds, took)
        }
      }
    }
  }

  test("interrupting a stalled stream ends it promptly") {
    withClient(Reply.ok(events).copy(stall = scala.concurrent.duration.FiniteDuration(10, "s"))) { (_, c) =>
      Clock.stopwatch.map { watch =>
        for {
          result <- Abort.run[Timeout](
            Async.timeout(500.millis)(Scope.run(c.stream(MessagesRequest("m", 64).user("hi")).run))
          )
          took <- watch.elapsed
        } yield {
          assert(result.isFailure, result)
          assert(took < 3.seconds, took)
        }
      }
    }
  }

  test("batch results stream, and every page of a list") {
    val lines = (1 to 40).map(i => s"""{"custom_id": "r$i", "result": {"type": "canceled"}}""").mkString("\n")
    def model(id: String) = s"""{"type": "model", "id": "$id", "display_name": "$id", "created_at": "2026-01-01T00:00:00Z"}"""
    val page1             = s"""{"data": [${model("m1")}, ${model("m2")}], "has_more": true, "first_id": "m1", "last_id": "m2"}"""
    val page2             = s"""{"data": [${model("m3")}], "has_more": false, "first_id": "m3", "last_id": "m3"}"""
    withClient(Reply.ok(lines), Reply.ok(page1), Reply.ok(page2)) { (api, c) =>
      for {
        results <- c.batchResults("msgbatch_1").run
        models  <- c.allModels(pageSize = 2).map(_.id).run
      } yield {
        assertEquals(results.map(_.customId).toList, (1 to 40).map(i => s"r$i").toList)
        assertEquals(models.toList, List("m1", "m2", "m3"))
        assertEquals(
          api.seen.map(_.route),
          Vector("GET /v1/messages/batches/msgbatch_1/results", "GET /v1/models?limit=2", "GET /v1/models?limit=2&after_id=m2")
        )
      }
    }
  }

  test("the tool loop runs a turn's calls concurrently, in call order, errors as results") {
    val calls =
      """{"id": "msg_t", "type": "message", "role": "assistant", "model": "m", "content": [
        |  {"type": "tool_use", "id": "t1", "name": "slow", "input": {}},
        |  {"type": "tool_use", "id": "t2", "name": "fast", "input": {}},
        |  {"type": "tool_use", "id": "t3", "name": "boom", "input": {}},
        |  {"type": "tool_use", "id": "t4", "name": "throw", "input": {}}
        |], "stop_reason": "tool_use", "stop_sequence": null, "usage": {"input_tokens": 10, "output_tokens": 5}}""".stripMargin
    withClient(Reply.ok(calls), Reply.ok(message("done"))) { (api, c) =>
      AtomicRef.init(Vector.empty[String]).map { finished =>
        val handler: ToolCall => ToolOutput < (Async & Abort[Throwable]) = call =>
          call.name match {
            case "slow"  => Async.sleep(200.millis).andThen(finished.updateAndGet(_ :+ "slow")).andThen(ToolOutput.text("s"))
            case "fast"  => finished.updateAndGet(_ :+ "fast").andThen(ToolOutput.text("f"))
            case "throw" => throw new IllegalArgumentException("thrown")
            case _       => Abort.fail(new IllegalStateException("kaput"))
          }
        for {
          run   <- c.runTools(MessagesRequest("m", 64).user("go"))(handler)
          order <- finished.get
        } yield {
          assert(run.isFinished)
          assertEquals(run.requests, 2)
          assertEquals(run.usage.inputTokens, 13L)
          assertEquals(run.message.text, "done")
          assertEquals(order, Vector("fast", "slow")) // ran concurrently
          assertEquals(
            api.seen(1).json("messages").at(2),
            Some(json("""{"role": "user", "content": [
              {"type": "tool_result", "tool_use_id": "t1", "content": "s"},
              {"type": "tool_result", "tool_use_id": "t2", "content": "f"},
              {"type": "tool_result", "tool_use_id": "t3", "content": "kaput", "is_error": true},
              {"type": "tool_result", "tool_use_id": "t4", "content": "thrown", "is_error": true}
            ]}"""))
          )
        }
      }
    }
  }

  test("awaitBatch polls until the batch has ended") {
    def batch(status: String) = s"""{"id": "b1", "type": "message_batch", "processing_status": "$status", "created_at": ""}"""
    withClient(Reply.ok(batch("in_progress")), Reply.ok(batch("in_progress")), Reply.ok(batch("ended"))) { (api, c) =>
      c.awaitBatch("b1", pollEvery = 10.millis).map { b =>
        assert(b.isEnded)
        assertEquals(api.hits, 3)
      }
    }
  }
}
