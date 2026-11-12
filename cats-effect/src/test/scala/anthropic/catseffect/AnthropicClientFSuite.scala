package anthropic.catseffect

import anthropic._
import anthropic.MockApi.{json, sse, Key}
import cats.effect.{IO, Ref}
import scala.concurrent.duration._

class AnthropicClientFSuite extends munit.CatsEffectSuite {

  def withClient[A](replies: Reply*)(f: (MockApi, AnthropicClientF[IO]) => IO[A]): IO[A] =
    cats.effect.Resource
      .make(IO(new MockApi(replies)))(api => IO(api.close()))
      .flatMap(api => AnthropicClientF[IO](ClientConfig.apiKey(Key).copy(baseUrl = api.url)).map(api -> _))
      .use { case (api, client) => f(api, client) }

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

  test("create and count tokens as IO") {
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

  test("a stream emits events, its text, and accumulates the final message") {
    withClient(Reply.ok(events)) { (_, c) =>
      val req = MessagesRequest("m", 64).user("hi")
      for {
        all  <- c.stream(req).compile.toVector
        text <- c.stream(req).through(AnthropicClientF.textDeltas).compile.string
        last <- c.stream(req).through(AnthropicClientF.accumulate).compile.lastOrError
      } yield {
        assertEquals(all.map(_.kind).last, "message_stop")
        assertEquals(text, "Bună ziua")
        assertEquals(last.text, "Bună ziua")
        assertEquals(last.usage.outputTokens, 4L)
        assertEquals(last.stopReason, Some(StopReason.EndTurn))
      }
    }
  }

  test("an error event fails the stream") {
    val body = sse(
      json("""{"type": "message_start", "message": {"id": "m", "type": "message", "role": "assistant", "model": "x", "content": [], "stop_reason": null, "usage": {}}}"""),
      json("""{"type": "error", "error": {"type": "overloaded_error", "message": "Overloaded"}}""")
    )
    withClient(Reply.ok(body)) { (_, c) =>
      c.stream(MessagesRequest("m", 64).user("hi")).compile.drain.attempt.map {
        case Left(e: AnthropicException.ApiError) => assertEquals(e.kind, "overloaded_error")
        case other                                 => fail(s"$other")
      }
    }
  }

  test("cancelling a slow call ends it promptly") {
    withClient(Reply.ok(message("late")).copy(delay = 10.seconds)) { (_, c) =>
      c.create(MessagesRequest("m", 64).user("hi")).timeout(300.millis).attempt.timed.map { case (took, result) =>
        assert(result.left.exists(_.isInstanceOf[java.util.concurrent.TimeoutException]), result)
        assert(took < 3.seconds, took)
      }
    }
  }

  test("batch results stream, and every page of a list") {
    val lines = Seq(
      """{"custom_id": "a", "result": {"type": "canceled"}}""",
      """{"custom_id": "b", "result": {"type": "expired"}}"""
    ).mkString("\n")
    def model(id: String) = s"""{"type": "model", "id": "$id", "display_name": "$id", "created_at": "2026-01-01T00:00:00Z"}"""
    val page1             = s"""{"data": [${model("m1")}, ${model("m2")}], "has_more": true, "first_id": "m1", "last_id": "m2"}"""
    val page2             = s"""{"data": [${model("m3")}], "has_more": false, "first_id": "m3", "last_id": "m3"}"""
    withClient(Reply.ok(lines), Reply.ok(page1), Reply.ok(page2)) { (api, c) =>
      for {
        results <- c.batchResults("msgbatch_1").compile.toVector
        models  <- c.allModels(pageSize = 2).map(_.id).compile.toVector
      } yield {
        assertEquals(results.map(_.customId), Vector("a", "b"))
        assertEquals(models, Vector("m1", "m2", "m3"))
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
      Ref.of[IO, Vector[String]](Vector.empty).flatMap { finished =>
        val handler: ToolCall => IO[ToolOutput] = call =>
          call.name match {
            case "slow" => IO.sleep(200.millis) >> finished.update(_ :+ "slow").as(ToolOutput.text("s"))
            case "fast" => finished.update(_ :+ "fast").as(ToolOutput.text("f"))
            case _      => IO.raiseError(new IllegalStateException("kaput"))
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
