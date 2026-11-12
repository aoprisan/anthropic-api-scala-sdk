package anthropic.zio2

import _root_.zio.{durationInt => _, Duration => ZDuration, Ref, Runtime, Unsafe, ZIO}
import anthropic._
import anthropic.MockApi.{json, sse, Key}
import scala.concurrent.Future
import scala.concurrent.duration._

class AnthropicClientZIOSuite extends munit.FunSuite {

  def withClient[A](replies: Reply*)(f: (MockApi, AnthropicClientZIO) => ZIO[Any, Throwable, A]): Future[A] = {
    val program = ZIO.scoped {
      for {
        api    <- ZIO.fromAutoCloseable(ZIO.attempt(new MockApi(replies)))
        client <- AnthropicClientZIO.scoped(ClientConfig.apiKey(Key).copy(baseUrl = api.url))
        a      <- f(api, client)
      } yield a
    }
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.runToFuture(program))
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

  test("create and count tokens as ZIO") {
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

  test("a failed call fails with the API error in the typed channel") {
    val error = """{"type": "error", "error": {"type": "invalid_request_error", "message": "nope"}}"""
    withClient(Reply(400, error)) { (_, c) =>
      c.create(MessagesRequest("m", 64).user("hi")).either.map {
        case Left(e: AnthropicException.ApiError) =>
          assertEquals(e.status, Some(400))
          assertEquals(e.kind, "invalid_request_error")
        case other => fail(s"$other")
      }
    }
  }

  test("the layer provides a client, and a bad configuration fails it") {
    val error = """{"type": "error", "error": {"type": "invalid_request_error", "message": "nope"}}"""
    withClient(Reply.ok(message("salut")), Reply(400, error)) { (api, _) =>
      val ask = ZIO.serviceWithZIO[AnthropicClientZIO](_.create(MessagesRequest("m", 64).user("hi")))
      for {
        m   <- ask.provide(AnthropicClientZIO.layer(ClientConfig.apiKey(Key).copy(baseUrl = api.url)))
        bad <- ask.provide(AnthropicClientZIO.layer(ClientConfig.apiKey(Key).copy(baseUrl = "not a url"))).either
      } yield {
        assertEquals(m.text, "salut")
        assert(bad.left.exists(_.isInstanceOf[AnthropicException.ConfigError]), bad)
      }
    }
  }

  test("a stream emits events, its text, and accumulates the final message") {
    withClient(Reply.ok(events)) { (_, c) =>
      val req = MessagesRequest("m", 64).user("hi")
      for {
        all  <- c.stream(req).runCollect
        text <- c.stream(req).via(AnthropicClientZIO.textDeltas).runFold("")(_ + _)
        last <- c.stream(req).via(AnthropicClientZIO.accumulate).runLast.some.orElseFail(new NoSuchElementException)
        // the pipeline holds no state between runs
        again <- c.stream(req).via(AnthropicClientZIO.accumulate).runLast
      } yield {
        assertEquals(all.map(_.kind).last, "message_stop")
        assertEquals(text, "Bună ziua")
        assertEquals(last.text, "Bună ziua")
        assertEquals(last.usage.outputTokens, 4L)
        assertEquals(last.stopReason, Some(StopReason.EndTurn))
        assertEquals(again.map(_.text), Some("Bună ziua"))
      }
    }
  }

  test("an error event fails the stream") {
    val body = sse(
      json("""{"type": "message_start", "message": {"id": "m", "type": "message", "role": "assistant", "model": "x", "content": [], "stop_reason": null, "usage": {}}}"""),
      json("""{"type": "error", "error": {"type": "overloaded_error", "message": "Overloaded"}}""")
    )
    withClient(Reply.ok(body)) { (_, c) =>
      c.stream(MessagesRequest("m", 64).user("hi")).runDrain.either.map {
        case Left(e: AnthropicException.ApiError) => assertEquals(e.kind, "overloaded_error")
        case other                                 => fail(s"$other")
      }
    }
  }

  test("interrupting a slow call ends it promptly") {
    withClient(Reply.ok(message("late")).copy(delay = 10.seconds)) { (_, c) =>
      c.create(MessagesRequest("m", 64).user("hi")).timeout(ZDuration.fromMillis(300)).timed.map { case (took, result) =>
        assertEquals(result, None)
        assert(took.toMillis < 3000, took)
      }
    }
  }

  test("interrupting a stalled stream ends it promptly") {
    withClient(Reply.ok(events).copy(stall = 10.seconds)) { (_, c) =>
      c.stream(MessagesRequest("m", 64).user("hi")).runCollect.timeout(ZDuration.fromMillis(500)).timed.map {
        case (took, result) =>
          assertEquals(result, None)
          assert(took.toMillis < 3000, took)
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
        results <- c.batchResults("msgbatch_1").runCollect
        models  <- c.allModels(pageSize = 2).map(_.id).runCollect
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
        |  {"type": "tool_use", "id": "t3", "name": "boom", "input": {}}
        |], "stop_reason": "tool_use", "stop_sequence": null, "usage": {"input_tokens": 10, "output_tokens": 5}}""".stripMargin
    withClient(Reply.ok(calls), Reply.ok(message("done"))) { (api, c) =>
      Ref.make(Vector.empty[String]).flatMap { finished =>
        val handler: ToolCall => ZIO[Any, Throwable, ToolOutput] = call =>
          call.name match {
            case "slow" => ZIO.sleep(ZDuration.fromMillis(200)) *> finished.update(_ :+ "slow").as(ToolOutput.text("s"))
            case "fast" => finished.update(_ :+ "fast").as(ToolOutput.text("f"))
            case _      => ZIO.fail(new IllegalStateException("kaput"))
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
              {"type": "tool_result", "tool_use_id": "t3", "content": "kaput", "is_error": true}
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
