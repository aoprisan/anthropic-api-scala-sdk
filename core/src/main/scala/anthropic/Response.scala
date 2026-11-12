package anthropic

import scala.collection.immutable.VectorMap

/** The message returned by `POST /v1/messages` (or accumulated from a stream).
  *
  * @param model       the model that produced the message (the fallback model after a refusal fallback)
  * @param stopDetails set when `stop_reason` is `refusal`: category, explanation, fallback credit...
  */
final case class Message(
    id: String,
    model: String,
    content: Vector[ContentBlock] = Vector.empty,
    stopReason: Option[StopReason] = None,
    stopSequence: Option[String] = None,
    stopDetails: Option[Json] = None,
    usage: Usage = Usage()
) {

  /** All text blocks, concatenated. */
  def text: String = content.collect { case ContentBlock.Text(t, _) => t }.mkString

  /** A safety classifier declined the request (and any fallback declined too). Check this before using
    * `content`.
    */
  def isRefusal: Boolean = stopReason.contains(StopReason.Refusal)

  /** The classifier category of a refusal (`"cyber"`, `"bio"`...), when it gives one. */
  def refusalCategory: Option[String] = stopDetails.flatMap(_.get("category")).flatMap(_.asString)

  /** The text parsed as JSON, for a request made with [[MessagesRequest.withJsonSchema]]. Decode it
    * further with your JSON library of choice (`Json.render` gives the text back).
    */
  def json: Either[AnthropicException.DecodeError, Json] =
    Json.parse(text).left.map(e => AnthropicException.DecodeError(s"structured output: $e"))

  /** Each cited text span with one of its citations, in order. */
  def citations: Vector[(String, Citation)] =
    content.flatMap {
      case ContentBlock.Text(t, cs) => cs.map(c => t -> c)
      case _                        => Vector.empty
    }

  /** The web search results in this message, across all searches. */
  def webSearchResults: Vector[WebSearchResult] =
    content.flatMap {
      case ContentBlock.WebSearchToolResult(_, WebSearchContent.Results(r), _) => r
      case _                                                                   => Vector.empty
    }

  /** Server tool calls that failed, with the id of the call: `(toolUseId, error)`. */
  def serverToolErrors: Vector[(String, ServerToolError)] =
    content.collect {
      case ContentBlock.WebSearchToolResult(id, WebSearchContent.Error(e), _)            => id -> e
      case ContentBlock.WebFetchToolResult(id, WebFetchContent.Error(e), _)              => id -> e
      case ContentBlock.BashCodeExecutionToolResult(id, CodeExecutionContent.Error(e), _) => id -> e
      case ContentBlock.CodeExecutionToolResult(id, CodeExecutionContent.Error(e), _)    => id -> e
      case ContentBlock.ToolSearchToolResult(id, ToolSearchContent.Error(e), _)          => id -> e
    }

  /** The tool calls the model asked for. */
  def toolUses: Vector[ToolCall] = content.collect { case ContentBlock.ToolUse(id, name, input) =>
    ToolCall(id, name, input)
  }

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("id" -> Json.Str(id)),
          Some("type" -> Json.Str("message")),
          Some("role" -> Json.Str("assistant")),
          Some("model" -> Json.Str(model)),
          Some("content" -> Json.Arr(content.map(_.toJson))),
          Some("stop_reason" -> stopReason.fold[Json](Json.Null)(r => Json.Str(r.value))),
          Some("stop_sequence" -> stopSequence.fold[Json](Json.Null)(Json.Str(_))),
          stopDetails.map(d => "stop_details" -> d),
          Some("usage" -> usage.toJson)
        ).flatten
      )
    )
}

object Message {
  def fromJson(j: Json): Message = {
    val c = Decode.obj(j, "message")
    Message(
      id = c.str("id"),
      model = c.str("model"),
      content = c.arr("content").map(ContentBlock.fromJson),
      stopReason = c.optStr("stop_reason").map(StopReason.apply),
      stopSequence = c.optStr("stop_sequence"),
      stopDetails = if (c.has("stop_details")) Some(c.json("stop_details")) else None,
      usage = if (c.has("usage")) Usage.fromJson(c.json("usage")) else Usage()
    )
  }
}

/** A block of a response. Any block type this SDK does not know is kept verbatim as [[ContentBlock.Other]],
  * so a new API feature never breaks decoding and an assistant turn echoes back intact. A known type
  * with a malformed shape is a decoding error.
  */
sealed trait ContentBlock extends Product with Serializable {

  /** The block's `type`. */
  def kind: String

  /** The `tool_use_id` of a server tool's result block. */
  def serverToolResultId: Option[String] = this match {
    case b: ContentBlock.WebSearchToolResult               => Some(b.toolUseId)
    case b: ContentBlock.WebFetchToolResult                => Some(b.toolUseId)
    case b: ContentBlock.BashCodeExecutionToolResult       => Some(b.toolUseId)
    case b: ContentBlock.CodeExecutionToolResult           => Some(b.toolUseId)
    case b: ContentBlock.TextEditorCodeExecutionToolResult => Some(b.toolUseId)
    case b: ContentBlock.ToolSearchToolResult              => Some(b.toolUseId)
    case _                                                 => None
  }

  def toJson: Json = {
    import ContentBlock._
    def typed(fields: (String, Json)*): Json.Obj = Json.Obj(VectorMap.from(("type" -> Json.Str(kind)) +: fields))
    def result(toolUseId: String, content: Json, extra: VectorMap[String, Json]): Json =
      Json.Obj(VectorMap.from(Vector("type" -> Json.Str(kind), "tool_use_id" -> Json.Str(toolUseId), "content" -> content)) ++ extra)
    this match {
      case Text(text, citations) =>
        if (citations.isEmpty) typed("text" -> Json.Str(text))
        else typed("text" -> Json.Str(text), "citations" -> Json.Arr(citations.map(_.toJson)))
      case Thinking(t, sig)    => typed("thinking" -> Json.Str(t), "signature" -> Json.Str(sig))
      case RedactedThinking(d) => typed("data" -> Json.Str(d))
      case ToolUse(id, name, input) =>
        typed("id" -> Json.Str(id), "name" -> Json.Str(name), "input" -> input)
      case ServerToolUse(id, name, input, extra) =>
        typed("id" -> Json.Str(id), "name" -> Json.Str(name), "input" -> input) ++ extra
      case WebSearchToolResult(id, content, extra)               => result(id, content.toJson, extra)
      case WebFetchToolResult(id, content, extra)                => result(id, content.toJson, extra)
      case BashCodeExecutionToolResult(id, content, extra)       => result(id, content.toJson, extra)
      case CodeExecutionToolResult(id, content, extra)           => result(id, content.toJson, extra)
      case TextEditorCodeExecutionToolResult(id, content, extra) => result(id, content, extra)
      case ToolSearchToolResult(id, content, extra)              => result(id, content.toJson, extra)
      case Fallback(from, to) =>
        typed("from" -> Json.obj("model" -> Json.Str(from.model)), "to" -> Json.obj("model" -> Json.Str(to.model)))
      case Other(json) => json
    }
  }
}

object ContentBlock {
  final case class Text(text: String, citations: Vector[Citation] = Vector.empty) extends ContentBlock {
    def kind = "text"
  }

  final case class Thinking(thinking: String, signature: String) extends ContentBlock { def kind = "thinking" }

  final case class RedactedThinking(data: String) extends ContentBlock { def kind = "redacted_thinking" }

  final case class ToolUse(id: String, name: String, input: Json) extends ContentBlock { def kind = "tool_use" }

  /** A call of a server tool (web search, web fetch, code execution...), run by the API. */
  final case class ServerToolUse(id: String, name: String, input: Json, extra: VectorMap[String, Json] = VectorMap.empty)
      extends ContentBlock { def kind = "server_tool_use" }

  final case class WebSearchToolResult(
      toolUseId: String,
      content: WebSearchContent,
      extra: VectorMap[String, Json] = VectorMap.empty
  ) extends ContentBlock { def kind = "web_search_tool_result" }

  final case class WebFetchToolResult(
      toolUseId: String,
      content: WebFetchContent,
      extra: VectorMap[String, Json] = VectorMap.empty
  ) extends ContentBlock { def kind = "web_fetch_tool_result" }

  final case class BashCodeExecutionToolResult(
      toolUseId: String,
      content: CodeExecutionContent,
      extra: VectorMap[String, Json] = VectorMap.empty
  ) extends ContentBlock { def kind = "bash_code_execution_tool_result" }

  /** The result of an older code execution version. */
  final case class CodeExecutionToolResult(
      toolUseId: String,
      content: CodeExecutionContent,
      extra: VectorMap[String, Json] = VectorMap.empty
  ) extends ContentBlock { def kind = "code_execution_tool_result" }

  /** A file the sandbox viewed, created or edited; `content` kept as sent. */
  final case class TextEditorCodeExecutionToolResult(
      toolUseId: String,
      content: Json,
      extra: VectorMap[String, Json] = VectorMap.empty
  ) extends ContentBlock { def kind = "text_editor_code_execution_tool_result" }

  final case class ToolSearchToolResult(
      toolUseId: String,
      content: ToolSearchContent,
      extra: VectorMap[String, Json] = VectorMap.empty
  ) extends ContentBlock { def kind = "tool_search_tool_result" }

  /** Marks where a refused attempt handed over to a fallback model. */
  final case class Fallback(from: ModelRef, to: ModelRef) extends ContentBlock { def kind = "fallback" }

  /** Any other block type, kept verbatim. */
  final case class Other(json: Json) extends ContentBlock {
    def kind: String = Decode.kind(json).getOrElse("")
  }

  /** The `type` tags this SDK decodes; any other is kept in [[Other]]. */
  val KnownTypes: Set[String] = Set(
    "text",
    "thinking",
    "redacted_thinking",
    "tool_use",
    "server_tool_use",
    "web_search_tool_result",
    "web_fetch_tool_result",
    "bash_code_execution_tool_result",
    "code_execution_tool_result",
    "text_editor_code_execution_tool_result",
    "tool_search_tool_result",
    "fallback"
  )

  def fromJson(j: Json): ContentBlock = Decode.kind(j) match {
    case Some(k) if KnownTypes(k) =>
      val c = Decode.obj(j, s"$k block")
      def extra = c.extra("type", "tool_use_id", "content")
      k match {
        case "text" =>
          Text(c.str("text"), c.arr("citations").map(Citation.fromJson))
        case "thinking"          => Thinking(c.strOr("thinking", ""), c.strOr("signature", ""))
        case "redacted_thinking" => RedactedThinking(c.str("data"))
        case "tool_use"          => ToolUse(c.str("id"), c.str("name"), c.json("input"))
        case "server_tool_use" =>
          ServerToolUse(c.str("id"), c.str("name"), c.json("input"), c.extra("type", "id", "name", "input"))
        case "web_search_tool_result" =>
          WebSearchToolResult(c.str("tool_use_id"), WebSearchContent.fromJson(c.req("content")), extra)
        case "web_fetch_tool_result" =>
          WebFetchToolResult(c.str("tool_use_id"), WebFetchContent.fromJson(c.req("content")), extra)
        case "bash_code_execution_tool_result" =>
          BashCodeExecutionToolResult(c.str("tool_use_id"), CodeExecutionContent.fromJson(c.req("content")), extra)
        case "code_execution_tool_result" =>
          CodeExecutionToolResult(c.str("tool_use_id"), CodeExecutionContent.fromJson(c.req("content")), extra)
        case "text_editor_code_execution_tool_result" =>
          TextEditorCodeExecutionToolResult(c.str("tool_use_id"), c.req("content"), extra)
        case "tool_search_tool_result" =>
          ToolSearchToolResult(c.str("tool_use_id"), ToolSearchContent.fromJson(c.req("content")), extra)
        case _ => // "fallback"
          Fallback(ModelRef.fromJson(c.req("from")), ModelRef.fromJson(c.req("to")))
      }
    case _ => Other(j)
  }
}

/** A server tool that ran but failed (`max_uses_exceeded`, `too_many_requests`, `unavailable`,
  * `url_not_accessible`...). The response itself still succeeds.
  *
  * @param extra `type` and anything else
  */
final case class ServerToolError(errorCode: String, extra: VectorMap[String, Json] = VectorMap.empty) {
  def toJson: Json = Json.Obj(extra + ("error_code" -> Json.Str(errorCode)))
}

object ServerToolError {

  /** An object with an `error_code`, or `None`. */
  private[anthropic] def unapply(j: Json): Option[ServerToolError] = j match {
    case Json.Obj(f) =>
      f.get("error_code").flatMap(_.asString).map(code => ServerToolError(code, f - "error_code"))
    case _ => None
  }
}

/** What a web search returned: a list of results, or an error object. */
sealed trait WebSearchContent extends Product with Serializable {
  def toJson: Json = this match {
    case WebSearchContent.Results(r) => Json.Arr(r.map(_.toJson))
    case WebSearchContent.Error(e)   => e.toJson
    case WebSearchContent.Other(j)   => j
  }
}

object WebSearchContent {
  final case class Results(results: Vector[WebSearchResult]) extends WebSearchContent
  final case class Error(error: ServerToolError)              extends WebSearchContent
  final case class Other(json: Json)                          extends WebSearchContent

  def fromJson(j: Json): WebSearchContent = j match {
    case Json.Arr(xs) =>
      try Results(xs.map(WebSearchResult.fromJson))
      catch { case _: DecodeFailure => Other(j) }
    case ServerToolError(e) => Error(e)
    case _                  => Other(j)
  }
}

/** @param pageAge          how old the page is, when known (e.g. `"2 days ago"`)
  * @param encryptedContent opaque page content; must be echoed back for later turns to cite it
  * @param extra            `type` and anything else
  */
final case class WebSearchResult(
    url: String,
    title: String = "",
    pageAge: Option[String] = None,
    encryptedContent: String = "",
    extra: VectorMap[String, Json] = VectorMap.empty
) {
  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("url" -> Json.Str(url)),
          Some("title" -> Json.Str(title)),
          pageAge.map(a => "page_age" -> Json.Str(a)),
          Some("encrypted_content" -> Json.Str(encryptedContent))
        ).flatten
      ) ++ extra
    )
}

object WebSearchResult {
  def fromJson(j: Json): WebSearchResult = {
    val c = Decode.obj(j, "web search result")
    WebSearchResult(
      c.str("url"),
      c.strOr("title", ""),
      c.optStr("page_age"),
      c.strOr("encrypted_content", ""),
      c.extra("url", "title", "page_age", "encrypted_content")
    )
  }
}

/** What a web fetch returned: the page, or an error object. */
sealed trait WebFetchContent extends Product with Serializable {
  def toJson: Json = this match {
    case WebFetchContent.Page(p)  => p.toJson
    case WebFetchContent.Error(e) => e.toJson
    case WebFetchContent.Other(j) => j
  }
}

object WebFetchContent {
  final case class Page(page: WebFetchResult)    extends WebFetchContent
  final case class Error(error: ServerToolError) extends WebFetchContent
  final case class Other(json: Json)             extends WebFetchContent

  def fromJson(j: Json): WebFetchContent =
    try Page(WebFetchResult.fromJson(j))
    catch {
      case _: DecodeFailure =>
        j match {
          case ServerToolError(e) => Error(e)
          case _                  => Other(j)
        }
    }
}

/** @param content a `document` block holding the page (`source.data` is its text for text pages)
  * @param extra   `type` and anything else
  */
final case class WebFetchResult(
    url: String,
    content: Json,
    retrievedAt: Option[String] = None,
    extra: VectorMap[String, Json] = VectorMap.empty
) {

  /** The fetched text, when the page came back as a text document. */
  def text: Option[String] = content.get("source").flatMap(_.get("data")).flatMap(_.asString)

  def title: Option[String] = content.get("title").flatMap(_.asString)

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(Some("url" -> Json.Str(url)), Some("content" -> content), retrievedAt.map(r => "retrieved_at" -> Json.Str(r))).flatten
      ) ++ extra
    )
}

object WebFetchResult {
  def fromJson(j: Json): WebFetchResult = {
    val c = Decode.obj(j, "web fetch result")
    WebFetchResult(c.str("url"), c.req("content"), c.optStr("retrieved_at"), c.extra("url", "content", "retrieved_at"))
  }
}

/** What a code execution returned: its output, or an error object. */
sealed trait CodeExecutionContent extends Product with Serializable {
  def toJson: Json = this match {
    case CodeExecutionContent.Output(o) => o.toJson
    case CodeExecutionContent.Error(e)  => e.toJson
    case CodeExecutionContent.Other(j)  => j
  }
}

object CodeExecutionContent {
  final case class Output(output: CodeExecutionOutput) extends CodeExecutionContent
  final case class Error(error: ServerToolError)       extends CodeExecutionContent
  final case class Other(json: Json)                   extends CodeExecutionContent

  def fromJson(j: Json): CodeExecutionContent =
    try Output(CodeExecutionOutput.fromJson(j))
    catch {
      case _: DecodeFailure =>
        j match {
          case ServerToolError(e) => Error(e)
          case _                  => Other(j)
        }
    }
}

/** @param returnCode `0` on success
  * @param content    files the code wrote: `{"type": ..., "file_id": ...}`, downloadable with
  *                   [[AnthropicClient.downloadFile]]
  * @param extra      `type` and anything else
  */
final case class CodeExecutionOutput(
    stdout: String,
    stderr: String,
    returnCode: Long,
    content: Vector[Json] = Vector.empty,
    extra: VectorMap[String, Json] = VectorMap.empty
) {

  /** Ids of the files the code wrote. */
  def fileIds: Vector[String] = content.flatMap(_.get("file_id").flatMap(_.asString))

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("stdout" -> Json.Str(stdout)),
          Some("stderr" -> Json.Str(stderr)),
          Some("return_code" -> Json.num(returnCode)),
          if (content.isEmpty) None else Some("content" -> Json.Arr(content))
        ).flatten
      ) ++ extra
    )
}

object CodeExecutionOutput {
  def fromJson(j: Json): CodeExecutionOutput = {
    val c = Decode.obj(j, "code execution output")
    CodeExecutionOutput(
      c.strOr("stdout", ""),
      c.strOr("stderr", ""),
      c.long("return_code"),
      c.arr("content"),
      c.extra("stdout", "stderr", "return_code", "content")
    )
  }
}

/** What a tool search found: references to the tools now loaded, or an error object. */
sealed trait ToolSearchContent extends Product with Serializable {
  def toJson: Json = this match {
    case ToolSearchContent.Found(f) => f.toJson
    case ToolSearchContent.Error(e) => e.toJson
    case ToolSearchContent.Other(j) => j
  }
}

object ToolSearchContent {
  final case class Found(found: ToolSearchFound) extends ToolSearchContent
  final case class Error(error: ServerToolError) extends ToolSearchContent
  final case class Other(json: Json)             extends ToolSearchContent

  def fromJson(j: Json): ToolSearchContent =
    try Found(ToolSearchFound.fromJson(j))
    catch {
      case _: DecodeFailure =>
        j match {
          case ServerToolError(e) => Error(e)
          case _                  => Other(j)
        }
    }
}

/** @param extra `type` and anything else */
final case class ToolSearchFound(toolReferences: Vector[ToolReference], extra: VectorMap[String, Json] = VectorMap.empty) {
  def toJson: Json = Json.Obj(VectorMap("tool_references" -> (Json.Arr(toolReferences.map(_.toJson)): Json)) ++ extra)
}

object ToolSearchFound {
  def fromJson(j: Json): ToolSearchFound = {
    val c = Decode.obj(j, "tool search result")
    ToolSearchFound(Decode.arr(c.req("tool_references"), "tool_references").map(ToolReference.fromJson), c.extra("tool_references"))
  }
}

/** @param extra `type` and anything else */
final case class ToolReference(toolName: String, extra: VectorMap[String, Json] = VectorMap.empty) {
  def toJson: Json = Json.Obj(VectorMap("tool_name" -> (Json.Str(toolName): Json)) ++ extra)
}

object ToolReference {
  def fromJson(j: Json): ToolReference = {
    val c = Decode.obj(j, "tool reference")
    ToolReference(c.str("tool_name"), c.extra("tool_name"))
  }
}

final case class ModelRef(model: String)

object ModelRef {
  def fromJson(j: Json): ModelRef = ModelRef(Decode.obj(j, "model ref").str("model"))
}

/** Where a cited span comes from. Any citation type this SDK does not know (search results...) is kept
  * verbatim as [[Citation.Other]].
  */
sealed trait Citation extends Product with Serializable {

  /** The quoted source text, when the citation type has one. */
  def citedTextOption: Option[String] = this match {
    case c: Citation.CharLocation            => Some(c.citedText)
    case c: Citation.PageLocation            => Some(c.citedText)
    case c: Citation.ContentBlockLocation    => Some(c.citedText)
    case c: Citation.WebSearchResultLocation => Some(c.citedText)
    case Citation.Other(v)                   => v.get("cited_text").flatMap(_.asString)
  }

  /** Index of the cited document among the request's documents. */
  def documentIndexOption: Option[Int] = this match {
    case c: Citation.CharLocation         => Some(c.documentIndex)
    case c: Citation.PageLocation         => Some(c.documentIndex)
    case c: Citation.ContentBlockLocation => Some(c.documentIndex)
    case _: Citation.WebSearchResultLocation => None
    case Citation.Other(v)                => v.get("document_index").flatMap(_.asInt)
  }

  /** The cited page of a web search result. */
  def urlOption: Option[String] = this match {
    case c: Citation.WebSearchResultLocation => Some(c.url)
    case Citation.Other(v)                   => v.get("url").flatMap(_.asString)
    case _                                   => None
  }

  def toJson: Json = {
    def title(t: Option[String]): Json = t.fold[Json](Json.Null)(Json.Str(_))
    def idx(i: Int): Json              = Json.num(i.toLong)
    this match {
      case Citation.CharLocation(text, doc, docTitle, start, end) =>
        Json.obj(
          "type"             -> Json.Str("char_location"),
          "cited_text"       -> Json.Str(text),
          "document_index"   -> idx(doc),
          "document_title"   -> title(docTitle),
          "start_char_index" -> idx(start),
          "end_char_index"   -> idx(end)
        )
      case Citation.PageLocation(text, doc, docTitle, start, end) =>
        Json.obj(
          "type"              -> Json.Str("page_location"),
          "cited_text"        -> Json.Str(text),
          "document_index"    -> idx(doc),
          "document_title"    -> title(docTitle),
          "start_page_number" -> idx(start),
          "end_page_number"   -> idx(end)
        )
      case Citation.ContentBlockLocation(text, doc, docTitle, start, end) =>
        Json.obj(
          "type"              -> Json.Str("content_block_location"),
          "cited_text"        -> Json.Str(text),
          "document_index"    -> idx(doc),
          "document_title"    -> title(docTitle),
          "start_block_index" -> idx(start),
          "end_block_index"   -> idx(end)
        )
      case Citation.WebSearchResultLocation(url, t, text, index) =>
        Json.obj(
          "type"            -> Json.Str("web_search_result_location"),
          "url"             -> Json.Str(url),
          "title"           -> title(t),
          "cited_text"      -> Json.Str(text),
          "encrypted_index" -> Json.Str(index)
        )
      case Citation.Other(v) => v
    }
  }
}

object Citation {

  /** Plain-text document: character range, end exclusive. */
  final case class CharLocation(
      citedText: String,
      documentIndex: Int,
      documentTitle: Option[String],
      startCharIndex: Int,
      endCharIndex: Int
  ) extends Citation

  /** PDF: page range, 1-indexed, end exclusive. */
  final case class PageLocation(
      citedText: String,
      documentIndex: Int,
      documentTitle: Option[String],
      startPageNumber: Int,
      endPageNumber: Int
  ) extends Citation

  /** Custom-content document: chunk range, end exclusive. */
  final case class ContentBlockLocation(
      citedText: String,
      documentIndex: Int,
      documentTitle: Option[String],
      startBlockIndex: Int,
      endBlockIndex: Int
  ) extends Citation

  /** A web search result.
    *
    * @param encryptedIndex opaque; must be echoed back unchanged
    */
  final case class WebSearchResultLocation(url: String, title: Option[String], citedText: String, encryptedIndex: String)
      extends Citation

  /** Any other citation type, kept verbatim. */
  final case class Other(json: Json) extends Citation

  val KnownTypes: Set[String] =
    Set("char_location", "page_location", "content_block_location", "web_search_result_location")

  def fromJson(j: Json): Citation = Decode.kind(j) match {
    case Some(k) if KnownTypes(k) =>
      val c = Decode.obj(j, s"$k citation")
      k match {
        case "char_location" =>
          CharLocation(c.str("cited_text"), c.int("document_index"), c.optStr("document_title"), c.int("start_char_index"), c.int("end_char_index"))
        case "page_location" =>
          PageLocation(c.str("cited_text"), c.int("document_index"), c.optStr("document_title"), c.int("start_page_number"), c.int("end_page_number"))
        case "content_block_location" =>
          ContentBlockLocation(c.str("cited_text"), c.int("document_index"), c.optStr("document_title"), c.int("start_block_index"), c.int("end_block_index"))
        case _ =>
          WebSearchResultLocation(c.str("url"), c.optStr("title"), c.strOr("cited_text", ""), c.strOr("encrypted_index", ""))
      }
    case _ => Other(j)
  }
}

/** Why the model stopped. Unknown reasons are kept in [[StopReason.Other]]. */
sealed abstract class StopReason(val value: String) extends Product with Serializable

object StopReason {
  case object EndTurn      extends StopReason("end_turn")
  case object MaxTokens    extends StopReason("max_tokens")
  case object StopSequence extends StopReason("stop_sequence")
  case object ToolUse      extends StopReason("tool_use")
  case object PauseTurn    extends StopReason("pause_turn")
  case object Refusal      extends StopReason("refusal")
  final case class Other(reason: String) extends StopReason(reason)

  def apply(value: String): StopReason = value match {
    case "end_turn"      => EndTurn
    case "max_tokens"    => MaxTokens
    case "stop_sequence" => StopSequence
    case "tool_use"      => ToolUse
    case "pause_turn"    => PauseTurn
    case "refusal"       => Refusal
    case other           => Other(other)
  }
}

/** @param cacheCreationInputTokens tokens written to the prompt cache by this request
  * @param cacheReadInputTokens     tokens read from the prompt cache; zero on every request means the prefix changes
  * @param serverToolUse            how many times each server tool ran
  * @param extra                    anything else (`iterations`, `service_tier`, `inference_geo`...)
  */
final case class Usage(
    inputTokens: Long = 0,
    outputTokens: Long = 0,
    cacheCreationInputTokens: Option[Long] = None,
    cacheReadInputTokens: Option[Long] = None,
    serverToolUse: Option[ServerToolUsage] = None,
    extra: VectorMap[String, Json] = VectorMap.empty
) {

  /** This usage plus another request's token and server tool counts; `extra` is left as is. */
  def +(other: Usage): Usage = {
    def sum(a: Option[Long], b: Option[Long]): Option[Long] =
      if (a.isEmpty && b.isEmpty) None else Some(a.getOrElse(0L) + b.getOrElse(0L))
    val tools = (serverToolUse, other.serverToolUse) match {
      case (s, None)          => s
      case (None, Some(o))    => Some(ServerToolUsage(o.webSearchRequests, o.webFetchRequests))
      case (Some(s), Some(o)) =>
        Some(s.copy(webSearchRequests = s.webSearchRequests + o.webSearchRequests, webFetchRequests = s.webFetchRequests + o.webFetchRequests))
    }
    copy(
      inputTokens = inputTokens + other.inputTokens,
      outputTokens = outputTokens + other.outputTokens,
      cacheCreationInputTokens = sum(cacheCreationInputTokens, other.cacheCreationInputTokens),
      cacheReadInputTokens = sum(cacheReadInputTokens, other.cacheReadInputTokens),
      serverToolUse = tools
    )
  }

  /** Applies the cumulative counts of a `message_delta` event. */
  private[anthropic] def merge(delta: VectorMap[String, Json]): Usage =
    delta.foldLeft(this) {
      case (u, (_, Json.Null))                        => u
      case (u, ("input_tokens", v))                   => u.copy(inputTokens = v.asLong.getOrElse(u.inputTokens))
      case (u, ("output_tokens", v))                  => u.copy(outputTokens = v.asLong.getOrElse(u.outputTokens))
      case (u, ("cache_creation_input_tokens", v))    => u.copy(cacheCreationInputTokens = v.asLong)
      case (u, ("cache_read_input_tokens", v))        => u.copy(cacheReadInputTokens = v.asLong)
      case (u, ("server_tool_use", v)) =>
        u.copy(serverToolUse =
          try Some(ServerToolUsage.fromJson(v))
          catch { case _: DecodeFailure => None }
        )
      case (u, (k, v)) => u.copy(extra = u.extra + (k -> v))
    }

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("input_tokens" -> Json.num(inputTokens)),
          Some("output_tokens" -> Json.num(outputTokens)),
          cacheCreationInputTokens.map(n => "cache_creation_input_tokens" -> Json.num(n)),
          cacheReadInputTokens.map(n => "cache_read_input_tokens" -> Json.num(n)),
          serverToolUse.map(s => "server_tool_use" -> s.toJson)
        ).flatten
      ) ++ extra
    )
}

object Usage {
  def fromJson(j: Json): Usage = {
    val c = Decode.obj(j, "usage")
    Usage(
      c.longOr("input_tokens", 0),
      c.longOr("output_tokens", 0),
      c.optLong("cache_creation_input_tokens"),
      c.optLong("cache_read_input_tokens"),
      if (c.has("server_tool_use")) Some(ServerToolUsage.fromJson(c.json("server_tool_use"))) else None,
      c.extra("input_tokens", "output_tokens", "cache_creation_input_tokens", "cache_read_input_tokens", "server_tool_use")
        .filterNot { case (_, v) => v.isNull }
    )
  }
}

/** `usage.server_tool_use`: billable server tool calls.
  *
  * @param extra anything else
  */
final case class ServerToolUsage(
    webSearchRequests: Long = 0,
    webFetchRequests: Long = 0,
    extra: VectorMap[String, Json] = VectorMap.empty
) {
  def toJson: Json =
    Json.Obj(
      VectorMap[String, Json]("web_search_requests" -> Json.num(webSearchRequests), "web_fetch_requests" -> Json.num(webFetchRequests)) ++ extra
    )
}

object ServerToolUsage {
  def fromJson(j: Json): ServerToolUsage = {
    val c = Decode.obj(j, "server_tool_use")
    ServerToolUsage(c.longOr("web_search_requests", 0), c.longOr("web_fetch_requests", 0), c.extra("web_search_requests", "web_fetch_requests"))
  }
}
