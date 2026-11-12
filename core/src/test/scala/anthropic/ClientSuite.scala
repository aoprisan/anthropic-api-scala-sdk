package anthropic

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import MockApi.{client, json, sse, Key}

/** The client against a local stand-in for the API: request shape and headers, citations, unknown block
  * types, server tools, retries, deadlines, streaming, token counting, batches, files, models and the
  * tool loop.
  */
class ClientSuite extends munit.FunSuite {

  def withApi[A](replies: Reply*)(f: MockApi => A): A = {
    val api = new MockApi(replies)
    try f(api)
    finally api.close()
  }

  val Cited: String =
    """{
      |  "id": "msg_1", "type": "message", "role": "assistant", "model": "claude-opus-5-5",
      |  "content": [
      |    {"type": "text", "text": "Cartea de identitate se eliberează în ", "citations": null},
      |    {"type": "text", "text": "30 de zile", "citations": [
      |      {"type": "char_location", "cited_text": "Termenul este de 30 de zile.", "document_index": 0,
      |       "document_title": "Ghid CI", "start_char_index": 0, "end_char_index": 28},
      |      {"type": "search_result_location", "cited_text": "30 de zile", "source": "https://hub.mai.gov.ro", "search_result_index": 0}
      |    ]},
      |    {"type": "brand_new_block", "id": "x_1", "payload": {"query": "ci"}},
      |    {"type": "text", "text": "."}
      |  ],
      |  "stop_reason": "end_turn", "stop_sequence": null,
      |  "usage": {"input_tokens": 1200, "output_tokens": 40, "cache_creation_input_tokens": 0, "cache_read_input_tokens": 900, "service_tier": "standard"}
      |}""".stripMargin

  def groundedRequest: MessagesRequest =
    MessagesRequest("claude-opus-5-5", 1500)
      .withSystemCached("Answer only from the documents.")
      .withEffort(Effort.Low)
      .withFallbacks(Fallbacks.Default)
      .message(
        MessageParam.user(
          ContentBlockParam.textDocument("Termenul este de 30 de zile.", "Ghid CI").withContext("hub.mai.gov.ro").withCitations,
          ContentBlockParam.text("În cât timp primesc buletinul?")
        )
      )

  test("sends the documented request shape and headers") {
    withApi(Reply.ok(Cited)) { api =>
      client(api).create(groundedRequest)
      val s = api.seen.head
      assertEquals(s.route, "POST /v1/messages")
      assertEquals(s.header("authorization"), Some(s"Bearer $Key"))
      assertEquals(s.header("x-api-key"), None)
      assertEquals(s.header("anthropic-version"), Some("2026-10-21"))
      assertEquals(s.header("anthropic-beta"), Some("server-side-fallback-2026-07-01"))
      assertEquals(s.header("content-type"), Some("application/json"))
      assert(s.header("user-agent").exists(_.startsWith("anthropic-api-scala-sdk/")), s.header("user-agent"))
      assertEquals(
        s.json,
        json("""{
          "model": "claude-opus-5-5",
          "max_tokens": 1500,
          "messages": [{"role": "user", "content": [
            {"type": "document", "source": {"type": "text", "media_type": "text/plain", "data": "Termenul este de 30 de zile."},
             "title": "Ghid CI", "context": "hub.mai.gov.ro", "citations": {"enabled": true}},
            {"type": "text", "text": "În cât timp primesc buletinul?"}
          ]}],
          "system": [{"type": "text", "text": "Answer only from the documents.", "cache_control": {"type": "ephemeral"}}],
          "output_config": {"effort": "low"},
          "fallbacks": "default"
        }""")
      )
    }
  }

  test("decodes citations and keeps unknown blocks for echoing") {
    withApi(Reply.ok(Cited)) { api =>
      val m = client(api).create(groundedRequest)
      assertEquals(m.text, "Cartea de identitate se eliberează în 30 de zile.")
      assertEquals(m.stopReason, Some(StopReason.EndTurn))
      assert(!m.isRefusal)
      assertEquals(m.usage.cacheReadInputTokens, Some(900L))
      assertEquals(m.usage.extra.get("service_tier"), Some(Json.Str("standard")))

      val cites = m.citations
      assertEquals(cites.length, 2)
      assertEquals(cites(0)._1, "30 de zile")
      cites(0)._2 match {
        case c: Citation.CharLocation =>
          assertEquals(c.documentIndex, 0)
          assertEquals(c.endCharIndex, 28)
        case other => fail(s"$other")
      }
      assertEquals(cites(1)._2.citedTextOption, Some("30 de zile"))
      assert(cites(1)._2.isInstanceOf[Citation.Other])

      // The unknown block survives a round trip unchanged, so an assistant turn echoes intact.
      m.content(2) match {
        case ContentBlock.Other(raw) => assertEquals(raw("type"), Json.Str("brand_new_block"))
        case other                   => fail(s"$other")
      }
      val v = MessageParam.assistant(m).toJson
      assertEquals(v("content").at(2), json(Cited)("content").at(2))
      assertEquals(v("content").at(1).get("citations").at(1).get("type"), Json.Str("search_result_location"))
      assertEquals(v("content").at(0).get.get("citations"), None)
    }
  }

  test("retries overload honouring retry-after, then succeeds") {
    val overloaded = """{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}"""
    withApi(Reply(529, overloaded).withHeaders("retry-after" -> "0"), Reply.ok(Cited)) { api =>
      val m = client(api, maxRetries = 2).create(groundedRequest)
      assertEquals(m.id, "msg_1")
      assertEquals(api.hits, 2)
    }
  }

  test("client errors are not retried and carry the request id but not the key") {
    val bad =
      """{"type":"error","error":{"type":"invalid_request_error","message":"max_tokens: too large"},"request_id":"req_body"}"""
    withApi(Reply(400, bad).withHeaders("request-id" -> "req_header")) { api =>
      val c   = client(api, maxRetries = 3)
      val err = intercept[AnthropicException.ApiError](c.create(groundedRequest))
      assertEquals(api.hits, 1)
      assertEquals(err.status, Some(400))
      assertEquals(err.kind, "invalid_request_error")
      assertEquals(err.message, "max_tokens: too large")
      assertEquals(err.requestId, Some("req_header"))
      assert(!err.isRetryable)
      assert(!s"$err ${err.getMessage}".contains(Key))
      assert(!c.toString.contains(Key), c.toString)
      assert(!c.config.toString.contains(Key))
    }
  }

  test("x-should-retry overrides the status") {
    withApi(Reply(500, "oops").withHeaders("x-should-retry" -> "false")) { api =>
      val err = intercept[AnthropicException.ApiError](client(api, maxRetries = 3).create(groundedRequest))
      assertEquals(api.hits, 1)
      assertEquals(err.status, Some(500))
      assertEquals(err.kind, "http_error")
    }
  }

  test("the deadline cuts slow attempts and retries short") {
    withApi(Reply.ok(Cited).copy(delay = 5.seconds)) { api =>
      val started = System.nanoTime()
      intercept[AnthropicException.DeadlineExceeded](
        client(api, maxRetries = 5).create(groundedRequest, CallOptions.within(300.millis))
      )
      assert((System.nanoTime() - started).nanos < 2.seconds)
    }
    // A retry that would end past the deadline is not attempted: the real error comes back.
    val busy = Reply(429, """{"type":"error","error":{"type":"rate_limit_error","message":"slow down"}}""")
      .withHeaders("retry-after" -> "10")
    withApi(busy) { api =>
      val err = intercept[AnthropicException.ApiError](
        client(api, maxRetries = 5).create(groundedRequest, CallOptions.within(2.seconds))
      )
      assertEquals(err.status, Some(429))
      assertEquals(api.hits, 1)
    }
  }

  test("fallback list, tools, thinking and bearer tokens") {
    withApi(Reply.ok(Cited)) { api =>
      val c = AnthropicClient(
        ClientConfig(Credential.Bearer("tok"), baseUrl = api.url, betas = Vector("oauth-2025-04-20"))
      )
      val schema = json(
        """{"type": "object", "properties": {"cui": {"type": "string"}}, "required": ["cui"], "additionalProperties": false}"""
      )
      val req = MessagesRequest("claude-sonnet-5-5", 512)
        .user("salut")
        .withThinking(ThinkingConfig.Adaptive(Some(ThinkingDisplay.Summarized)))
        .withFallbacks(Fallbacks.models("claude-opus-4-8"))
        .tool(Tool("cui", "Checks a CUI", schema).withStrict.withCacheControl(CacheControl.oneHour))
        .tool(json("""{"type": "web_search_20260209", "name": "web_search"}"""))
        .beta("server-side-fallback-2026-06-01")
        .param("inference_geo", Json.Str("eu"))
      c.create(req)

      val s    = api.seen.head
      val body = s.json
      assertEquals(s.header("authorization"), Some("Bearer tok"))
      assertEquals(s.header("x-api-key"), None)
      assertEquals(s.header("anthropic-beta"), Some("oauth-2025-04-20,server-side-fallback-2026-06-01"))
      assertEquals(body("messages").at(0), Some(json("""{"role": "user", "content": "salut"}""")))
      assertEquals(body("thinking"), json("""{"type": "adaptive", "display": "summarized"}"""))
      assertEquals(body("fallbacks"), json("""[{"model": "claude-opus-4-8"}]"""))
      assertEquals(body("tools").at(0).get.apply("cache_control"), json("""{"type": "ephemeral", "ttl": "1h"}"""))
      assertEquals(body("tools").at(0).get.apply("strict"), Json.True)
      assertEquals(body("tools").at(1), Some(json("""{"type": "web_search_20260209", "name": "web_search"}""")))
      assertEquals(body("inference_geo"), Json.Str("eu"))
      assertEquals(body.get("stream"), None)
    }
  }

  test("refusals are visible before content") {
    val refused =
      """{"id":"msg_r","type":"message","role":"assistant","model":"claude-opus-5-5","content":[],
        |"stop_reason":"refusal","stop_sequence":null,"stop_details":{"type":"refusal","category":null,"explanation":"declined"},
        |"usage":{"input_tokens":10,"output_tokens":0}}""".stripMargin
    withApi(Reply.ok(refused)) { api =>
      val m = client(api).create(groundedRequest)
      assert(m.isRefusal)
      assertEquals(m.stopDetails.map(_("explanation")), Some(Json.Str("declined")))
      assertEquals(m.refusalCategory, None)
    }
  }

  test("system messages, task budgets, structured outputs and eager tools") {
    val structured =
      """{"id":"msg_j","type":"message","role":"assistant","model":"claude-opus-5-5",
        |"content":[{"type":"text","text":"{\"days\": 30}"}],"stop_reason":"end_turn","stop_sequence":null,
        |"usage":{"input_tokens":10,"output_tokens":5}}""".stripMargin
    val refused =
      """{"id":"msg_r","type":"message","role":"assistant","model":"claude-opus-5-5","content":[],
        |"stop_reason":"refusal","stop_sequence":null,"stop_details":{"type":"refusal","category":"cyber","explanation":null},
        |"usage":{"input_tokens":10,"output_tokens":0}}""".stripMargin
    withApi(Reply.ok(structured), Reply.ok(refused)) { api =>
      val schema = json(
        """{"type": "object", "properties": {"days": {"type": "integer"}}, "required": ["days"], "additionalProperties": false}"""
      )
      val request = MessagesRequest("claude-opus-5-5", 20000)
        .withEffort(Effort.High)
        .withTaskBudget(64000)
        .withJsonSchema(schema)
        .beta("task-budgets-2026-03-13")
        .tool(Tool("lookup", "Look up a term", json("""{"type": "object"}""")).withEagerInputStreaming)
        .user("În cât timp primesc buletinul?")
        .message(MessageParam.system("Answer in JSON."))
      val c = client(api)
      val m = c.create(request)
      assertEquals(m.json.map(_("days").asInt), Right(Some(30)))

      val s = api.seen.head
      assertEquals(s.header("anthropic-beta"), Some("task-budgets-2026-03-13"))
      assertEquals(
        s.json("output_config"),
        Json.obj(
          "effort"      -> Json.Str("high"),
          "format"      -> Json.obj("type" -> Json.Str("json_schema"), "schema" -> schema),
          "task_budget" -> json("""{"type": "tokens", "total": 64000}""")
        )
      )
      assertEquals(s.json("tools").at(0).get.apply("eager_input_streaming"), Json.True)
      assertEquals(s.json("messages").at(1), Some(json("""{"role": "system", "content": "Answer in JSON."}""")))

      val refusal = c.create(MessagesRequest("claude-opus-5-5", 100).user("x"))
      assertEquals(refusal.refusalCategory, Some("cyber"))
      assert(refusal.json.isLeft)
    }
  }

  def streamBody: String = sse(
    json("""{"type": "message_start", "message": {"id": "msg_s", "type": "message", "role": "assistant", "model": "claude-opus-5-5", "content": [],
           "stop_reason": null, "stop_sequence": null, "usage": {"input_tokens": 50, "output_tokens": 1, "cache_read_input_tokens": 40}}}"""),
    json("""{"type": "content_block_start", "index": 0, "content_block": {"type": "thinking", "thinking": "", "signature": ""}}"""),
    json("""{"type": "content_block_delta", "index": 0, "delta": {"type": "thinking_delta", "thinking": ""}}"""),
    json("""{"type": "content_block_delta", "index": 0, "delta": {"type": "signature_delta", "signature": "sig"}}"""),
    json("""{"type": "content_block_stop", "index": 0}"""),
    json("""{"type": "ping"}"""),
    json("""{"type": "content_block_start", "index": 1, "content_block": {"type": "text", "text": ""}}"""),
    json("""{"type": "content_block_delta", "index": 1, "delta": {"type": "text_delta", "text": "În 30 "}}"""),
    json("""{"type": "content_block_delta", "index": 1, "delta": {"type": "text_delta", "text": "de zile"}}"""),
    json("""{"type": "content_block_delta", "index": 1, "delta": {"type": "citations_delta", "citation": {"type": "char_location",
           "cited_text": "Termenul este de 30 de zile.", "document_index": 0, "document_title": "Ghid CI", "start_char_index": 0, "end_char_index": 28}}}"""),
    json("""{"type": "content_block_stop", "index": 1}"""),
    json("""{"type": "something_new", "detail": 1}"""),
    json("""{"type": "content_block_start", "index": 2, "content_block": {"type": "tool_use", "id": "toolu_1", "name": "cui", "input": {}}}"""),
    json("""{"type": "content_block_delta", "index": 2, "delta": {"type": "input_json_delta", "partial_json": "{\"cui\": \"RO1"}}"""),
    json("""{"type": "content_block_delta", "index": 2, "delta": {"type": "input_json_delta", "partial_json": "8547290\"}"}}"""),
    json("""{"type": "content_block_stop", "index": 2}"""),
    json("""{"type": "message_delta", "delta": {"stop_reason": "tool_use", "stop_sequence": null}, "usage": {"output_tokens": 77}}"""),
    json("""{"type": "message_stop"}""")
  )

  test("streams events and accumulates the final message") {
    val reply = Reply.ok(streamBody).withHeaders("content-type" -> "text/event-stream", "request-id" -> "req_s")
    withApi(reply) { api =>
      val c = client(api)
      val s = c.stream(groundedRequest)
      assertEquals(s.requestId, Some("req_s"))
      val text  = new StringBuilder
      val kinds = Vector.newBuilder[String]
      s.foreach { e =>
        e match {
          case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(t)) => text.append(t)
          case _                                                     => ()
        }
        kinds += e.toJson("type").asString.get
      }
      val all = kinds.result()
      assertEquals(text.toString, "În 30 de zile")
      assert(all.contains("something_new"))
      assertEquals(all.last, "message_stop")
      assertEquals(api.seen.head.json("stream"), Json.True)
      assertEquals(s.snapshot.map(_.text), Some("În 30 de zile"))

      val m = c.stream(groundedRequest).finalMessage()
      assertEquals(m.text, "În 30 de zile")
      assertEquals(m.stopReason, Some(StopReason.ToolUse))
      assertEquals(m.usage.inputTokens, 50L)
      assertEquals(m.usage.outputTokens, 77L)
      assertEquals(m.usage.cacheReadInputTokens, Some(40L))
      assertEquals(m.citations.length, 1)
      assertEquals(m.content(0), ContentBlock.Thinking("", "sig"))
      assertEquals(m.toolUses, Vector(ToolCall("toolu_1", "cui", json("""{"cui": "RO18547290"}"""))))
    }
  }

  test("an error event or a cut stream fails the final message") {
    val start =
      json("""{"type": "message_start", "message": {"id": "m", "type": "message", "role": "assistant", "model": "x", "content": [], "stop_reason": null, "usage": {}}}""")
    withApi(Reply.ok(sse(start, json("""{"type": "error", "error": {"type": "overloaded_error", "message": "Overloaded"}}""")))) { api =>
      val err = intercept[AnthropicException.ApiError](client(api).stream(groundedRequest).finalMessage())
      assertEquals(err.status, None)
      assertEquals(err.kind, "overloaded_error")
      assert(err.isRetryable)
    }
    withApi(Reply.ok(sse(start))) { api =>
      intercept[AnthropicException.DecodeError](client(api).stream(groundedRequest).finalMessage())
    }
  }

  test("opening a stream retries, but errors after it opens do not") {
    withApi(Reply(503, "").withHeaders("retry-after" -> "0"), Reply.ok(streamBody)) { api =>
      val m = client(api, maxRetries = 1).stream(groundedRequest).finalMessage()
      assertEquals(m.id, "msg_s")
      assertEquals(api.hits, 2)
    }
  }

  test("a deadline cuts a stream that stalls mid-body") {
    val start =
      json("""{"type": "message_start", "message": {"id": "m", "type": "message", "role": "assistant", "model": "x", "content": [], "stop_reason": null, "usage": {}}}""")
    withApi(Reply.ok(sse(start)).copy(stall = 5.seconds)) { api =>
      val started = System.nanoTime()
      val s       = client(api).stream(groundedRequest, CallOptions.within(500.millis))
      assertEquals(s.next().kind, "message_start")
      intercept[AnthropicException.DeadlineExceeded](s.next())
      assert((System.nanoTime() - started).nanos < 3.seconds)
    }
  }

  test("counts tokens with only the prompt fields") {
    withApi(Reply.ok("""{"input_tokens": 321}""")) { api =>
      assertEquals(client(api).countTokens(groundedRequest), 321L)
      val s = api.seen.head
      assertEquals(s.route, "POST /v1/messages/count_tokens")
      assertEquals(s.json("model"), Json.Str("claude-opus-5-5"))
      assertEquals(s.json.get("max_tokens"), None)
      assertEquals(s.json.get("fallbacks"), None)
      assert(s.json.get("system").isDefined)
    }
  }

  test("server tools are sent as documented") {
    withApi(Reply.ok(Cited)) { api =>
      val req = MessagesRequest("claude-opus-5-5", 4096)
        .user("Ce acte îmi trebuie pentru pașaport?")
        .tool(
          WebSearchTool()
            .withMaxUses(3)
            .withAllowedDomains("mai.gov.ro")
            .withUserLocation(UserLocation(country = Some("RO"), timezone = Some("Europe/Bucharest")))
        )
        .tool(WebFetchTool().withCitations.withMaxContentTokens(20000))
        .tool(CodeExecutionTool())
        .tool(ToolSearchTool.bm25)
        .tool(Tool("cui", "Checks a CUI", json("""{"type": "object"}""")).deferred)
      client(api).create(req)
      val s = api.seen.head
      assertEquals(
        s.json("tools"),
        json("""[
          {"type": "web_search_20260209", "name": "web_search", "max_uses": 3, "allowed_domains": ["mai.gov.ro"],
           "user_location": {"type": "approximate", "country": "RO", "timezone": "Europe/Bucharest"}},
          {"type": "web_fetch_20260209", "name": "web_fetch", "citations": {"enabled": true}, "max_content_tokens": 20000},
          {"type": "code_execution_20260521", "name": "code_execution"},
          {"type": "tool_search_tool_bm25_20251119", "name": "tool_search_tool_bm25"},
          {"name": "cui", "description": "Checks a CUI", "input_schema": {"type": "object"}, "defer_loading": true}
        ]""")
      )
      assertEquals(s.header("anthropic-beta"), None)
    }
  }

  val Searched: Json = json("""{
    "id": "msg_w", "type": "message", "role": "assistant", "model": "claude-opus-5-5",
    "content": [
      {"type": "server_tool_use", "id": "srvtoolu_1", "name": "web_search", "input": {"query": "acte pasaport"}},
      {"type": "web_search_tool_result", "tool_use_id": "srvtoolu_1", "content": [
        {"type": "web_search_result", "url": "https://pasapoarte.mai.gov.ro/acte", "title": "Acte necesare",
         "encrypted_content": "EqQBCkY", "page_age": "3 days ago", "brand_new_field": 7}
      ]},
      {"type": "server_tool_use", "id": "srvtoolu_2", "name": "web_search", "input": {"query": "taxa"}},
      {"type": "web_search_tool_result", "tool_use_id": "srvtoolu_2",
       "content": {"type": "web_search_tool_result_error", "error_code": "max_uses_exceeded"}},
      {"type": "text", "text": "Buletinul și taxa achitată.", "citations": [
        {"type": "web_search_result_location", "url": "https://pasapoarte.mai.gov.ro/acte", "title": "Acte necesare",
         "encrypted_index": "Eo8BCi", "cited_text": "Cartea de identitate și dovada plății taxei."}
      ]},
      {"type": "server_tool_use", "id": "srvtoolu_3", "name": "web_fetch", "input": {"url": "https://pasapoarte.mai.gov.ro/acte"}},
      {"type": "web_fetch_tool_result", "tool_use_id": "srvtoolu_3", "content": {
        "type": "web_fetch_result", "url": "https://pasapoarte.mai.gov.ro/acte", "retrieved_at": "2026-10-04T07:00:00Z",
        "content": {"type": "document", "source": {"type": "text", "media_type": "text/plain", "data": "Acte necesare: ..."},
                    "title": "Acte necesare", "citations": {"enabled": true}}}},
      {"type": "server_tool_use", "id": "srvtoolu_4", "name": "bash_code_execution", "input": {"command": "python plot.py"}},
      {"type": "bash_code_execution_tool_result", "tool_use_id": "srvtoolu_4", "content": {
        "type": "bash_code_execution_result", "stdout": "ok\n", "stderr": "", "return_code": 0,
        "content": [{"type": "bash_code_execution_output", "file_id": "file_011"}]}},
      {"type": "tool_search_tool_result", "tool_use_id": "srvtoolu_5", "content": {
        "type": "tool_search_tool_search_result", "tool_references": [{"type": "tool_reference", "tool_name": "cui"}]}}
    ],
    "stop_reason": "end_turn", "stop_sequence": null,
    "usage": {"input_tokens": 5000, "output_tokens": 120, "server_tool_use": {"web_search_requests": 2, "web_fetch_requests": 1}}
  }""")

  test("server tool blocks decode typed and echo back unchanged") {
    withApi(Reply.ok(Searched.render)) { api =>
      val m = client(api).create(MessagesRequest("claude-opus-5-5", 4096).user("?"))
      m.content(0) match {
        case ContentBlock.ServerToolUse(_, name, input, _) =>
          assertEquals(name, "web_search")
          assertEquals(input("query"), Json.Str("acte pasaport"))
        case other => fail(s"$other")
      }
      val results = m.webSearchResults
      assertEquals(results.length, 1)
      assertEquals(results(0).title, "Acte necesare")
      assertEquals(results(0).pageAge, Some("3 days ago"))
      val errors = m.serverToolErrors
      assertEquals(errors.map { case (id, e) => (id, e.errorCode) }, Vector(("srvtoolu_2", "max_uses_exceeded")))
      assert(m.content(3) match {
        case ContentBlock.WebSearchToolResult(_, WebSearchContent.Error(_), _) => true
        case _                                                                => false
      })

      val (span, cite) = m.citations.head
      assertEquals(span, "Buletinul și taxa achitată.")
      assert(cite.isInstanceOf[Citation.WebSearchResultLocation])
      assertEquals(cite.urlOption, Some("https://pasapoarte.mai.gov.ro/acte"))
      assertEquals(cite.citedTextOption, Some("Cartea de identitate și dovada plății taxei."))
      assertEquals(cite.documentIndexOption, None)

      m.content(6) match {
        case ContentBlock.WebFetchToolResult(_, WebFetchContent.Page(page), _) =>
          assertEquals(page.text, Some("Acte necesare: ..."))
          assertEquals(page.title, Some("Acte necesare"))
        case other => fail(s"$other")
      }
      m.content(8) match {
        case ContentBlock.BashCodeExecutionToolResult(_, CodeExecutionContent.Output(out), _) =>
          assertEquals((out.stdout, out.returnCode), ("ok\n", 0L))
          assertEquals(out.fileIds, Vector("file_011"))
        case other => fail(s"$other")
      }
      m.content(9) match {
        case ContentBlock.ToolSearchToolResult(_, ToolSearchContent.Found(f), _) =>
          assertEquals(f.toolReferences.head.toolName, "cui")
        case other => fail(s"$other")
      }
      val usage = m.usage.serverToolUse.get
      assertEquals((usage.webSearchRequests, usage.webFetchRequests), (2L, 1L))

      // Every block, unknown fields included, goes back exactly as received.
      assertEquals(MessageParam.assistant(m).toJson("content"), Searched("content"))
      assertEquals(m.usage.toJson("server_tool_use"), Searched("usage")("server_tool_use"))
      assertEquals(Message.fromJson(m.toJson), m)
    }
  }

  test("streamed server tool calls accumulate their input") {
    val body = sse(
      json("""{"type": "message_start", "message": {"id": "m", "type": "message", "role": "assistant", "model": "x", "content": [], "stop_reason": null, "usage": {}}}"""),
      json("""{"type": "content_block_start", "index": 0, "content_block": {"type": "server_tool_use", "id": "srvtoolu_1", "name": "web_search", "input": {}}}"""),
      json("""{"type": "content_block_delta", "index": 0, "delta": {"type": "input_json_delta", "partial_json": "{\"query\": "}}"""),
      json("""{"type": "content_block_delta", "index": 0, "delta": {"type": "input_json_delta", "partial_json": "\"taxa\"}"}}"""),
      json("""{"type": "content_block_stop", "index": 0}"""),
      Json.obj("type" -> Json.Str("content_block_start"), "index" -> Json.num(1L), "content_block" -> Searched("content").at(1).get),
      json("""{"type": "content_block_stop", "index": 1}"""),
      json("""{"type": "message_delta", "delta": {"stop_reason": "end_turn"}, "usage": {"output_tokens": 9, "server_tool_use": {"web_search_requests": 1}}}"""),
      json("""{"type": "message_stop"}""")
    )
    withApi(Reply.ok(body)) { api =>
      val m = client(api).stream(groundedRequest).finalMessage()
      m.content(0) match {
        case ContentBlock.ServerToolUse(_, _, input, _) => assertEquals(input, json("""{"query": "taxa"}"""))
        case other                                      => fail(s"$other")
      }
      assertEquals(m.webSearchResults.length, 1)
      assertEquals(m.usage.serverToolUse.map(_.webSearchRequests), Some(1L))
    }
  }

  def batchJson(status: String): String =
    s"""{"id": "msgbatch_1", "type": "message_batch", "processing_status": "$status",
       |"request_counts": {"processing": 0, "succeeded": 1, "errored": 2, "canceled": 1, "expired": 0},
       |"created_at": "2026-10-04T07:00:00Z", "expires_at": "2026-10-05T07:00:00Z", "ended_at": null,
       |"cancel_initiated_at": null, "archived_at": null, "results_url": null}""".stripMargin

  test("batches: create, poll, list, cancel, delete") {
    val page = s"""{"data": [${batchJson("ended")}], "has_more": true, "first_id": "msgbatch_1", "last_id": "msgbatch_1"}"""
    withApi(
      Reply.ok(batchJson("in_progress")),
      Reply.ok(batchJson("ended")),
      Reply.ok(page),
      Reply.ok(batchJson("canceling")),
      Reply.ok("""{"id": "msgbatch_1", "type": "message_batch_deleted"}""")
    ) { api =>
      val c   = client(api)
      val req = MessagesRequest("claude-opus-5-5", 64).user("a").beta("context-management-2025-06-27")
      val created = c.createBatch(
        Seq(BatchRequest("r-1", req), BatchRequest("r-2", MessagesRequest("claude-haiku-4-5", 64).user("b")))
      )
      assertEquals(created.processingStatus, ProcessingStatus.InProgress)
      assertEquals(created.requestCounts.errored, 2L)
      assertEquals(created.extra.get("type"), Some(Json.Str("message_batch")))

      assert(c.batch("msgbatch_1").isEnded)
      val params = ListParams.limit(1)
      val listed = c.listBatches(params)
      assertEquals(listed.data.head.id, "msgbatch_1")
      assertEquals(listed.nextParams(params).flatMap(_.afterId), Some("msgbatch_1"))
      assertEquals(c.cancelBatch("msgbatch_1").processingStatus, ProcessingStatus.Canceling)
      assertEquals(c.deleteBatch("msgbatch_1").extra.get("type"), Some(Json.Str("message_batch_deleted")))

      val seen = api.seen
      assertEquals(
        seen.map(_.route),
        Vector(
          "POST /v1/messages/batches",
          "GET /v1/messages/batches/msgbatch_1",
          "GET /v1/messages/batches?limit=1",
          "POST /v1/messages/batches/msgbatch_1/cancel",
          "DELETE /v1/messages/batches/msgbatch_1"
        )
      )
      assertEquals(seen(0).header("anthropic-beta"), Some("context-management-2025-06-27"))
      assertEquals(
        seen(0).json("requests").at(0),
        Some(json("""{"custom_id": "r-1", "params": {"model": "claude-opus-5-5", "max_tokens": 64, "messages": [{"role": "user", "content": "a"}]}}"""))
      )
      assertEquals(seen(0).json("requests").at(1).get.apply("custom_id"), Json.Str("r-2"))
      assertEquals(seen(1).header("content-type"), None)
    }
  }

  test("batch results stream line by line") {
    val message = json(Cited)
    val lines = Vector(
      Json.obj("custom_id" -> Json.Str("r-2"), "result" -> Json.obj("type" -> Json.Str("succeeded"), "message" -> message)),
      json("""{"custom_id": "r-1", "result": {"type": "errored", "error": {"type": "error", "error": {"type": "invalid_request_error", "message": "bad"}}}}"""),
      json("""{"custom_id": "r-3", "result": {"type": "errored", "error": {"type": "overloaded_error", "message": "busy"}}}"""),
      json("""{"custom_id": "r-4", "result": {"type": "canceled"}}"""),
      json("""{"custom_id": "r-5", "result": {"type": "expired"}}"""),
      json("""{"custom_id": "r-6", "result": {"type": "postponed", "until": "later"}}""")
    )
    // The last line has no trailing newline.
    withApi(Reply.ok(lines.map(_.render).mkString("\n"))) { api =>
      val results = client(api).batchResults("msgbatch_1").all()
      assertEquals(api.seen.head.path, "/v1/messages/batches/msgbatch_1/results")
      assertEquals(results.length, 6)
      results(0).result match {
        case BatchOutcome.Succeeded(m) => assertEquals(m.citations.length, 2)
        case other                     => fail(s"$other")
      }
      assertEquals(results(1).result.error.map(_.kind), Some("invalid_request_error"))
      assertEquals(results(2).result.error.map(_.kind), Some("overloaded_error"))
      assertEquals(results(3).result, BatchOutcome.Canceled)
      assertEquals(results(4).result, BatchOutcome.Expired)
      assertEquals(results(5).result.toJson, lines(5)("result"))
    }
  }

  val File: String =
    """{"id": "file_011", "type": "file", "filename": "ghid.pdf", "mime_type": "application/pdf", "size_bytes": 4,
      |"created_at": "2026-10-04T07:00:00Z", "downloadable": false}""".stripMargin

  test("files: upload, list, get, download, delete") {
    val page = s"""{"data": [$File], "has_more": false, "first_id": "file_011", "last_id": "file_011"}"""
    withApi(
      Reply.ok(File),
      Reply.ok(page),
      Reply.ok(File),
      Reply.ok("%PDF"),
      Reply.ok("""{"id": "file_011", "type": "file_deleted"}""")
    ) { api =>
      val c        = client(api)
      val uploaded = c.uploadFile("ghid.pdf", "application/pdf", "%PDF".getBytes("UTF-8"))
      assertEquals((uploaded.id, uploaded.sizeBytes, uploaded.downloadable), ("file_011", 4L, false))
      val params = ListParams(limit = Some(5), afterId = Some("file_000"))
      val listed = c.listFiles(params)
      assertEquals(listed.data.head.filename, "ghid.pdf")
      assertEquals(listed.nextParams(params), None)
      assertEquals(c.file("../v1/x?y").mimeType, "application/pdf")
      assertEquals(new String(c.downloadFile("file_011"), "UTF-8"), "%PDF")
      assertEquals(c.deleteFile("file_011").id, "file_011")

      val seen = api.seen
      assertEquals(
        seen.map(_.route),
        Vector(
          "POST /v1/files",
          "GET /v1/files?limit=5&after_id=file_000",
          // An id cannot leave its path segment.
          "GET /v1/files/..%2Fv1%2Fx%3Fy",
          "GET /v1/files/file_011/content",
          "DELETE /v1/files/file_011"
        )
      )
      assert(seen(0).header("content-type").exists(_.startsWith("multipart/form-data; boundary=")))
      assertEquals(seen(0).header("authorization"), Some(s"Bearer $Key"))
      val upload = seen(0).raw
      assert(upload.contains("Content-Disposition: form-data; name=\"file\"; filename=\"ghid.pdf\""), upload)
      assert(upload.contains("Content-Type: application/pdf\r\n\r\n%PDF\r\n"), upload)

      assertEquals(
        ContentBlockParam.fileDocument(uploaded.id).toJson,
        json("""{"type": "document", "source": {"type": "file", "file_id": "file_011"}}""")
      )
    }
  }

  test("models: list and retrieve with capabilities") {
    val model = json("""{"type": "model", "id": "claude-opus-5-5", "display_name": "Claude Opus 5.5", "created_at": "2026-09-01T00:00:00Z",
        "max_input_tokens": 1000000, "max_tokens": 128000,
        "capabilities": {"image_input": {"supported": true}, "thinking": {"supported": true, "types": {"enabled": {"supported": false}, "adaptive": {"supported": true}}},
                         "effort": {"supported": true, "max": {"supported": true}}}}""")
    val page = Json.obj(
      "data"     -> Json.arr(model),
      "has_more" -> Json.False,
      "first_id" -> Json.Str("claude-opus-5-5"),
      "last_id"  -> Json.Str("claude-opus-5-5")
    )
    withApi(Reply.ok(page.render), Reply.ok(model.render)) { api =>
      val c = client(api)
      assertEquals(c.listModels().data.length, 1)
      val m = c.model("claude-opus-5-5")
      assertEquals((m.maxInputTokens, m.maxTokens), (Some(1000000L), Some(128000L)))
      assert(m.supports("image_input"))
      assert(m.supports("thinking", "types", "adaptive"))
      assert(!m.supports("thinking", "types", "enabled"))
      assert(m.supports("effort", "max"))
      assert(!m.supports("pdf_input"))
      assertEquals(m.toJson, model)
      assertEquals(api.seen.map(_.route), Vector("GET /v1/models", "GET /v1/models/claude-opus-5-5"))
    }
  }

  def turn(stopReason: String, content: String, inputTokens: Long): Reply =
    Reply.ok(
      s"""{"id": "msg_t", "type": "message", "role": "assistant", "model": "claude-opus-5-5", "content": $content,
         |"stop_reason": "$stopReason", "stop_sequence": null, "usage": {"input_tokens": $inputTokens, "output_tokens": 5}}""".stripMargin
    )

  def cuiRequest: MessagesRequest =
    MessagesRequest("claude-opus-5-5", 1024)
      .tool(Tool("cui", "Checks a CUI", json("""{"type": "object", "properties": {"cui": {"type": "string"}}, "required": ["cui"]}""")))
      .user("Check RO1 and RO2")

  def checkCui(cui: String): ToolOutput = {
    if (cui == "RO1") Thread.sleep(50)
    if (cui == "RO1") ToolOutput.text("active") else throw new IllegalArgumentException(s"unknown CUI $cui")
  }

  test("the tool loop runs parallel calls and resumes paused turns") {
    withApi(
      turn(
        "tool_use",
        """[{"type": "thinking", "thinking": "", "signature": "sig"},
          | {"type": "tool_use", "id": "toolu_1", "name": "cui", "input": {"cui": "RO1"}},
          | {"type": "tool_use", "id": "toolu_2", "name": "cui", "input": {"cui": "RO2"}}]""".stripMargin,
        10
      ),
      turn("pause_turn", """[{"type": "server_tool_use", "id": "srvtoolu_1", "name": "web_search", "input": {"query": "RO2"}}]""", 20),
      turn("end_turn", """[{"type": "text", "text": "RO1 is active."}]""", 30)
    ) { api =>
      val calls = new ConcurrentLinkedQueue[String]()
      val run = client(api).runTools(cuiRequest) { call =>
        calls.add(call.name)
        checkCui(call.input("cui").asString.getOrElse(""))
      }
      assert(run.isFinished)
      assertEquals(run.requests, 3)
      assertEquals(run.message.text, "RO1 is active.")
      assertEquals((run.usage.inputTokens, run.usage.outputTokens), (60L, 15L))
      assertEquals(calls.asScala.toVector, Vector("cui", "cui"))
      assertEquals(run.request.messages.length, 5)

      val second = api.seen(1).json("messages")
      assertEquals(second.at(1).get.apply("role"), Json.Str("assistant"))
      assertEquals(second.at(1).get.apply("content").at(0), Some(json("""{"type": "thinking", "thinking": "", "signature": "sig"}""")))
      // Both results in one user turn, in call order although RO1 finished last.
      assertEquals(
        second.at(2),
        Some(json("""{"role": "user", "content": [
          {"type": "tool_result", "tool_use_id": "toolu_1", "content": "active"},
          {"type": "tool_result", "tool_use_id": "toolu_2", "content": "unknown CUI RO2", "is_error": true}
        ]}"""))
      )
      // A paused turn is sent back as the last message, with no user turn after it.
      val third = api.seen(2).json("messages").asArray.get
      assertEquals(third.length, 4)
      assertEquals(third(3)("role"), Json.Str("assistant"))
      assertEquals(third(3)("content").at(0).get.apply("type"), Json.Str("server_tool_use"))
    }
  }

  test("a tool loop cut short resumes with the pending calls") {
    withApi(
      turn("tool_use", """[{"type": "tool_use", "id": "toolu_1", "name": "cui", "input": {"cui": "RO1"}}]""", 10),
      turn("end_turn", """[{"type": "text", "text": "Done."}]""", 10)
    ) { api =>
      val c   = client(api)
      val run = c.runTools(cuiRequest, ToolLoopOptions(maxRequests = 1))(_ => ToolOutput.text("never"))
      assert(!run.isFinished)
      assertEquals(run.message.stopReason, Some(StopReason.ToolUse))
      assertEquals(run.request.messages.length, 2)

      val resumed = c.runTools(run.request)(call => ToolOutput.text(s"${call.input("cui").asString.get} ok"))
      assert(resumed.isFinished)
      assertEquals(resumed.requests, 1)
      assertEquals(
        api.seen(1).json("messages").at(2),
        Some(json("""{"role": "user", "content": [{"type": "tool_result", "tool_use_id": "toolu_1", "content": "RO1 ok"}]}"""))
      )
    }
  }

  test("API keys go as bearer without the OAuth beta, or as x-api-key on request") {
    withApi(Reply.ok(Cited)) { api =>
      client(api).create(MessagesRequest("claude-opus-5-5", 64).user("salut"))
      val c = AnthropicClient(ClientConfig(Credential.XApiKey(Key), baseUrl = api.url))
      c.create(MessagesRequest("claude-opus-5-5", 64).user("salut"))
      assert(!c.toString.contains(Key))
      val seen = api.seen
      assertEquals(seen(0).header("anthropic-beta"), None)
      assertEquals(seen(1).header("x-api-key"), Some(Key))
      assertEquals(seen(1).header("authorization"), None)
      assertEquals(seen(1).header("anthropic-beta"), None)
    }
  }

  test("requests API version 2026-10-21 rejects are not sent") {
    withApi(Reply.ok(Cited)) { api =>
      val c = client(api)
      val cited = ContentBlockParam.Text(
        "30 de zile",
        citations = Some(Vector(Citation.Other(json("""{"type": "char_location", "cited_text": "30 de zile"}"""))))
      )
      val searchResult = json("""{"type": "search_result", "source": "https://mai.gov.ro", "title": "Ghid", "content": [
        {"type": "text", "text": "30 de zile", "citations": [{"type": "char_location", "cited_text": "30"}]}
      ]}""")
      val bad = Vector(
        MessagesRequest("claude-sonnet-4-6", 64).user("salut").param("temperature", Json.num(0.2)),
        MessagesRequest("claude-sonnet-4-6", 64).user("salut").param("top_k", Json.num(5L)),
        MessagesRequest("claude-opus-5-5", 64)
          .user("salut")
          .message(MessageParam.assistant(ContentBlockParam.textDocument("Termenul este de 30 de zile.", "Ghid CI"))),
        MessagesRequest("claude-opus-5-5", 64).message(MessageParam.user(cited)),
        MessagesRequest("claude-opus-5-5", 64).message(
          MessageParam.user(
            ContentBlockParam.ToolResult("toolu_1", ToolResultContent.Blocks(Vector(ContentBlockParam.Raw(searchResult))))
          )
        )
      )
      bad.foreach { request =>
        intercept[AnthropicException.InvalidRequest](c.create(request))
        intercept[AnthropicException.InvalidRequest](c.countTokens(request))
        intercept[AnthropicException.InvalidRequest](c.stream(request))
        assert(request.validate.isLeft)
      }
      val err = intercept[AnthropicException.InvalidRequest](c.createBatch(Seq(BatchRequest("r1", bad(0)))))
      assert(err.getMessage.contains("r1: `temperature`"), err.getMessage)
      assertEquals(api.hits, 0)

      // Still accepted: citations on echoed assistant text, documents in user turns, citations enabled.
      c.create(groundedRequest.message(MessageParam.assistant(cited)).user("Mulțumesc"))

      // On the legacy version nothing is checked and the old header goes out.
      val legacy =
        AnthropicClient(ClientConfig.apiKey(Key).copy(baseUrl = api.url, apiVersion = AnthropicClient.LegacyApiVersion))
      legacy.create(bad(0))
      val seen = api.seen
      assertEquals(seen(1).header("anthropic-version"), Some("2023-06-01"))
      assertEquals(seen(1).json("temperature"), Json.num(0.2))
    }
  }

  test("messages are typed by role") {
    val system = MessageParam.System(SystemPrompt.Blocks(Vector(SystemBlock("Be brief."))))
    assertEquals(system.toJson, json("""{"role": "system", "content": [{"type": "text", "text": "Be brief."}]}"""))
    assertEquals(system.role, Role.System)
    assertEquals(system.content, None)
    val user = MessageParam.userText("salut")
    assertEquals(user.role, Role.User)
    assertEquals(user.content, Some(Content.Text("salut")))
    assertEquals(MessageParam.assistant(Vector.empty[ContentBlockParam]).toJson, json("""{"role": "assistant", "content": []}"""))
  }

  test("configuration from the environment and base URL checks") {
    val env = Map("ANTHROPIC_API_KEY" -> " ", "ANTHROPIC_AUTH_TOKEN" -> "tok", "ANTHROPIC_BASE_URL" -> "https://gw.example/anthropic/")
    val c   = ClientConfig.fromEnv(env.get).get
    assertEquals(c.credential, Credential.Bearer("tok"))
    assertEquals(c.baseUrl, "https://gw.example/anthropic/")
    assertEquals(ClientConfig.fromEnv(Map.empty[String, String].get), None)
    assertEquals(
      Http.url(c.baseUrl, Seq("v1", "models"), Seq("limit" -> "2")).toString,
      "https://gw.example/anthropic/v1/models?limit=2"
    )
    intercept[AnthropicException.ConfigError](AnthropicClient(ClientConfig.apiKey("k").copy(baseUrl = "not a url")))
    intercept[AnthropicException.ConfigError](AnthropicClient(ClientConfig.apiKey("k").copy(baseUrl = "ftp://x")))
  }
}
