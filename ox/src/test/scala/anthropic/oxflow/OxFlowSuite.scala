package anthropic.oxflow

import _root_.ox.{sleep, timeoutOption}
import anthropic.*
import anthropic.MockApi.{json, sse, Key}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class OxFlowSuite extends munit.FunSuite {

  def withClient[A](replies: Reply*)(f: (MockApi, AnthropicClient) => A): A =
    scala.util.Using.resource(new MockApi(replies)) { api =>
      scala.util.Using.resource(AnthropicClient(ClientConfig.apiKey(Key).copy(baseUrl = api.url)))(f(api, _))
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

  test("a flow emits events, its text, and accumulates the final message, once per run") {
    withClient(Reply.ok(events)) { (api, c) =>
      val flow = c.streamFlow(MessagesRequest("m", 64).user("hi"))
      assertEquals(api.hits, 0) // nothing is sent until the flow runs
      val all  = flow.runToList()
      val text = flow.textDeltas.runToList().mkString
      val last = flow.accumulate.runLast()
      assertEquals(api.hits, 3)
      assertEquals(all.map(_.kind).last, "message_stop")
      assertEquals(text, "Bună ziua")
      assertEquals(last.text, "Bună ziua")
      assertEquals(last.usage.outputTokens, 4L)
      assertEquals(last.stopReason, Some(StopReason.EndTurn))
    }
  }

  test("an error event fails the flow") {
    val body = sse(
      json("""{"type": "message_start", "message": {"id": "m", "type": "message", "role": "assistant", "model": "x", "content": [], "stop_reason": null, "usage": {}}}"""),
      json("""{"type": "error", "error": {"type": "overloaded_error", "message": "Overloaded"}}""")
    )
    withClient(Reply.ok(body)) { (_, c) =>
      val e = intercept[AnthropicException.ApiError](c.streamFlow(MessagesRequest("m", 64).user("hi")).runDrain())
      assertEquals(e.kind, "overloaded_error")
    }
  }

  test("a timeout ends a slow call promptly") {
    withClient(Reply.ok(message("late")).copy(delay = 10.seconds)) { (_, c) =>
      val start  = System.nanoTime()
      val result = timeoutOption(300.millis)(c.create(MessagesRequest("m", 64).user("hi")))
      assertEquals(result, None)
      assert((System.nanoTime() - start).nanos < 3.seconds)
    }
  }

  test("a timeout ends a stalled flow promptly") {
    withClient(Reply.ok(events).copy(stall = 10.seconds)) { (_, c) =>
      val start  = System.nanoTime()
      val result = timeoutOption(500.millis)(c.streamFlow(MessagesRequest("m", 64).user("hi")).runToList())
      assertEquals(result, None)
      assert((System.nanoTime() - start).nanos < 3.seconds)
    }
  }

  test("batch results flow, and every page of a list, fetched as consumed") {
    val lines = (1 to 40).map(i => s"""{"custom_id": "r$i", "result": {"type": "canceled"}}""").mkString("\n")
    def model(id: String) = s"""{"type": "model", "id": "$id", "display_name": "$id", "created_at": "2026-01-01T00:00:00Z"}"""
    val page1             = s"""{"data": [${model("m1")}, ${model("m2")}], "has_more": true, "first_id": "m1", "last_id": "m2"}"""
    val page2             = s"""{"data": [${model("m3")}], "has_more": false, "first_id": "m3", "last_id": "m3"}"""
    withClient(Reply.ok(lines), Reply.ok(page1), Reply.ok(page1), Reply.ok(page2)) { (api, c) =>
      val results = c.batchResultsFlow("msgbatch_1").runToList()
      val first   = c.allModels(pageSize = 2).map(_.id).take(1).runToList()
      assertEquals(api.hits, 2) // the second page was not asked for
      val models = c.allModels(pageSize = 2).map(_.id).runToList()
      assertEquals(results.map(_.customId), (1 to 40).map(i => s"r$i").toList)
      assertEquals(first, List("m1"))
      assertEquals(models, List("m1", "m2", "m3"))
      assertEquals(api.seen.map(_.route).takeRight(2), Vector("GET /v1/models?limit=2", "GET /v1/models?limit=2&after_id=m2"))
    }
  }

  val calls: String =
    """{"id": "msg_t", "type": "message", "role": "assistant", "model": "m", "content": [
      |  {"type": "tool_use", "id": "t1", "name": "slow", "input": {}},
      |  {"type": "tool_use", "id": "t2", "name": "fast", "input": {}},
      |  {"type": "tool_use", "id": "t3", "name": "boom", "input": {}}
      |], "stop_reason": "tool_use", "stop_sequence": null, "usage": {"input_tokens": 10, "output_tokens": 5}}""".stripMargin

  test("the scoped tool loop runs a turn's calls concurrently, in call order, errors as results") {
    withClient(Reply.ok(calls), Reply.ok(message("done"))) { (api, c) =>
      val finished = new ConcurrentLinkedQueue[String]()
      val run = c.runToolsScoped(MessagesRequest("m", 64).user("go")) { call =>
        call.name match {
          case "slow" => sleep(200.millis); finished.add("slow"); ToolOutput.text("s")
          case "fast" => finished.add("fast"); ToolOutput.text("f")
          case _      => throw new IllegalStateException("kaput")
        }
      }
      assert(run.isFinished)
      assertEquals(run.requests, 2)
      assertEquals(run.usage.inputTokens, 13L)
      assertEquals(run.message.text, "done")
      assertEquals(finished.asScala.toList, List("fast", "slow")) // ran concurrently
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

  test("interrupting the scoped tool loop interrupts its handlers") {
    withClient(Reply.ok(calls), Reply.ok(message("done"))) { (api, c) =>
      val interrupted = new ConcurrentLinkedQueue[String]()
      val result = timeoutOption(300.millis) {
        c.runToolsScoped(MessagesRequest("m", 64).user("go")) { call =>
          try { sleep(10.seconds); ToolOutput.text("late") }
          catch { case e: InterruptedException => interrupted.add(call.name); throw e }
        }
      }
      assertEquals(result, None)
      assertEquals(interrupted.asScala.toList.sorted, List("boom", "fast", "slow"))
      assertEquals(api.hits, 1)
    }
  }
}
