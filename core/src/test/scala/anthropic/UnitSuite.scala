package anthropic

import MockApi.json

class UnitSuite extends munit.FunSuite {

  test("JSON round-trips text, numbers, escapes and field order") {
    // A JSON `\u0001` escape, written so both Scala versions read the literal the same way.
    val text = "{\"b\":1,\"a\":[true,false,null,-2.5e3,\"é\\n\\\"\\" + "u0001\"],\"c\":{\"x\":12345678901234567890,\"y\":0.2}}"
    val j    = json(text)
    assertEquals(j.asObject.map(_.keys.toList), Some(List("b", "a", "c")))
    assertEquals(j("a").at(3).flatMap(_.asDouble), Some(-2500.0))
    assertEquals(j("a").at(4).flatMap(_.asString), Some("é\n\"\u0001"))
    assertEquals(j("c")("x").asLong, None) // too big for a Long, still a number
    assertEquals(json(j.render), j)
    assertEquals(Json.num(64000L).render, "64000")
    assertEquals(Json.num(0.2).render, "0.2")
    assertEquals(json("1"), json("1.0"))
  }

  test("malformed JSON is rejected with an offset, deep nesting without a stack overflow") {
    assert(Json.parse("""{"a":}""").left.exists(_.contains("offset")))
    assert(Json.parse("[1,2").isLeft)
    assert(Json.parse("1 2").isLeft)
    assert(Json.parse("[" * 10000).isLeft)
  }

  test("SSE events split across chunks and line endings") {
    val p                               = new SseParser
    def push(s: String): Vector[String] = { val b = s.getBytes("UTF-8"); p.push(b, b.length) }
    assertEquals(push("event: ping\ndata: {\"type\":"), Vector.empty)
    assertEquals(
      push("\"ping\"}\n\n: comment\n\nevent: x\r\ndata: a\r\ndata: b\r\n\r\ndata:c"),
      Vector("""{"type":"ping"}""", "a\nb")
    )
    assertEquals(p.finish(), Some("c"))
  }

  test("a multi-byte character split across chunks survives") {
    val p     = new SseParser
    val bytes = "data: În\n\n".getBytes("UTF-8")
    val cut   = 7 // inside the two bytes of "Î"
    assertEquals(p.push(bytes.take(cut), cut), Vector.empty)
    assertEquals(p.push(bytes.drop(cut), bytes.length - cut), Vector("În"))
  }

  test("unknown events and deltas are kept; a known event with a bad shape is an error") {
    val e = StreamEvent.fromJson(json("""{"type": "brand_new", "x": 1}"""))
    assertEquals(e, StreamEvent.Other(json("""{"type": "brand_new", "x": 1}""")))
    assertEquals(e.kind, "brand_new")
    val d = Delta.fromJson(json("""{"type": "future_delta", "y": 2}"""))
    assertEquals(d.toJson, json("""{"type": "future_delta", "y": 2}"""))
    intercept[DecodeFailure](StreamEvent.fromJson(json("""{"type": "content_block_stop"}""")))
  }

  test("usage adds up and merges cumulative deltas") {
    val a = Usage(10, 5, cacheReadInputTokens = Some(4), serverToolUse = Some(ServerToolUsage(1, 0)))
    val b = Usage(20, 7, cacheCreationInputTokens = Some(3), serverToolUse = Some(ServerToolUsage(2, 1)))
    assertEquals(a + b, Usage(30, 12, Some(3), Some(4), Some(ServerToolUsage(3, 1))))
    val merged = Usage(50, 1).merge(json("""{"output_tokens": 77, "input_tokens": null, "iterations": 2}""").asObject.get)
    assertEquals(merged, Usage(50, 77, extra = scala.collection.immutable.VectorMap("iterations" -> Json.num(2L))))
  }

  test("beta header values are deduplicated, in order") {
    val r = MessagesRequest("m", 1)
      .beta(MessagesRequest.BetaTaskBudget)
      .beta("x")
      .withFallbacks(Fallbacks.Default)
      .withTaskBudget(30000)
    assertEquals(r.betaHeaderValues, Vector(MessagesRequest.BetaTaskBudget, "x", MessagesRequest.BetaFallbackDefault))
  }

  test("path segments are percent-encoded") {
    assertEquals(Http.encodeSegment("file_011"), "file_011")
    assertEquals(Http.encodeSegment("../v1/x?y"), "..%2Fv1%2Fx%3Fy")
    assertEquals(Http.encodeSegment(".."), "%2E%2E")
    assertEquals(Http.encodeSegment("a b#ș"), "a%20b%23%C8%99")
  }

  test("builders echo assistant turns and set document options") {
    val doc = ContentBlockParam.chunkedDocument(Seq("a", "b"), "T").withCitations.withCacheControl(CacheControl.ephemeral)
    assertEquals(
      doc.toJson,
      json("""{"type": "document", "source": {"type": "content", "content": [{"type": "text", "text": "a"}, {"type": "text", "text": "b"}]},
              "title": "T", "citations": {"enabled": true}, "cache_control": {"type": "ephemeral"}}""")
    )
    assertEquals(ContentBlockParam.text("x").withCitations, ContentBlockParam.text("x"))
    assertEquals(
      ContentBlockParam.toolResult("t1", "boom", isError = true).toJson,
      json("""{"type": "tool_result", "tool_use_id": "t1", "content": "boom", "is_error": true}""")
    )
    assertEquals(ToolChoice.Specific("cui").toJson, json("""{"type": "tool", "name": "cui"}"""))
    assertEquals(ToolChoice.NoTools.toJson, json("""{"type": "none"}"""))
    assertEquals(ThinkingConfig.BetweenTools.toJson, json("""{"type": "between_tools"}"""))
    assertEquals(
      Fallbacks.Models(Vector(FallbackModel("a", Some(10)))).toJson,
      json("""[{"model": "a", "max_tokens": 10}]""")
    )
  }

  test("tool outputs from Either and Try") {
    assertEquals(ToolOutput.fromEither(Left("no")), ToolOutput.error("no"))
    assertEquals(ToolOutput.fromTry(scala.util.Try("ok")), ToolOutput.text("ok"))
    assertEquals(ToolOutput.fromTry(scala.util.Failure(new RuntimeException("bad"))), ToolOutput.error("bad"))
  }
}
