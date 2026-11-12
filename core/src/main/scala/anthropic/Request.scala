package anthropic

import scala.collection.immutable.VectorMap

/** The body of `POST /v1/messages`. Immutable: every builder method returns a new request.
  *
  * {{{
  * val request = MessagesRequest("claude-opus-5-5", 2048)
  *   .withSystemCached("Answer only from the documents and cite them.")
  *   .message(MessageParam.user(
  *     ContentBlockParam.textDocument("Cartea de identitate se eliberează în 30 de zile.", "Ghid CI").withCitations,
  *     ContentBlockParam.text("În cât timp primesc buletinul?")
  *   ))
  * }}}
  *
  * @param cacheControl top-level automatic caching: the API places the breakpoint on the last cacheable block
  * @param fallbacks    server-side retry of a refused request on another model; the client adds the beta header
  *                     this form needs
  * @param betas        further `anthropic-beta` values for this request (not part of the body)
  * @param extra        any other top-level parameter, sent as is (e.g. `inference_geo`, `metadata`)
  */
final case class MessagesRequest(
    model: String,
    maxTokens: Int,
    messages: Vector[MessageParam] = Vector.empty,
    system: Option[SystemPrompt] = None,
    thinking: Option[ThinkingConfig] = None,
    outputConfig: Option[OutputConfig] = None,
    tools: Vector[ToolDefinition] = Vector.empty,
    toolChoice: Option[ToolChoice] = None,
    stopSequences: Vector[String] = Vector.empty,
    cacheControl: Option[CacheControl] = None,
    fallbacks: Option[Fallbacks] = None,
    betas: Vector[String] = Vector.empty,
    extra: VectorMap[String, Json] = VectorMap.empty
) {

  def withSystem(text: String): MessagesRequest = copy(system = Some(SystemPrompt.Text(text)))

  /** A system prompt marked as a cache breakpoint: keep it byte-identical across requests (no dates or
    * ids in it) so later requests read it from the cache.
    */
  def withSystemCached(text: String): MessagesRequest =
    copy(system = Some(SystemPrompt.Blocks(Vector(SystemBlock(text, Some(CacheControl.ephemeral))))))

  def message(message: MessageParam): MessagesRequest = copy(messages = messages :+ message)

  def addMessages(more: Iterable[MessageParam]): MessagesRequest = copy(messages = messages ++ more)

  /** A user turn holding `text`. */
  def user(text: String): MessagesRequest = message(MessageParam.userText(text))

  def withThinking(thinking: ThinkingConfig): MessagesRequest = copy(thinking = Some(thinking))

  private def output: OutputConfig = outputConfig.getOrElse(OutputConfig())

  /** Sets `output_config.effort`. Not every model accepts it: Claude Haiku 4.5 and Claude Sonnet 4.5
    * reject the parameter (use `ThinkingConfig.Enabled` there), Claude Opus 4.5 takes only
    * `Low`/`Medium`/`High`, and Claude Opus 4.6 / Sonnet 4.6 do not take `Xhigh`. The SDK does not
    * check this; the API returns an error.
    */
  def withEffort(effort: Effort): MessagesRequest = copy(outputConfig = Some(output.copy(effort = Some(effort))))

  /** Sets `output_config.task_budget`: a token budget for the whole agentic loop that the model sees and
    * paces itself against (unlike `max_tokens`, which cuts it off). At least 20,000. The client adds the
    * beta header it needs. Leave `remaining` unset unless you rewrite history between requests.
    */
  def withTaskBudget(total: Int): MessagesRequest =
    copy(outputConfig = Some(output.copy(taskBudget = Some(TaskBudget(total)))))

  /** Structured outputs: the reply's text is JSON valid against `schema` (which needs
    * `additionalProperties: false` on its objects). Read it with [[Message.json]]. Cannot be combined
    * with citations. On models that reject a forced `tool_choice` this replaces a forced tool call made
    * only to get JSON back.
    */
  def withJsonSchema(schema: Json): MessagesRequest =
    copy(outputConfig =
      Some(output.copy(format = Some(Json.obj("type" -> Json.Str("json_schema"), "schema" -> schema))))
    )

  def tool(tool: ToolDefinition): MessagesRequest = copy(tools = tools :+ tool)

  /** A tool definition given as raw JSON. */
  def tool(raw: Json): MessagesRequest = tool(ToolDefinition.Raw(raw))

  def withToolChoice(choice: ToolChoice): MessagesRequest = copy(toolChoice = Some(choice))

  def withStopSequences(sequences: String*): MessagesRequest = copy(stopSequences = sequences.toVector)

  def withCacheControl(cc: CacheControl): MessagesRequest = copy(cacheControl = Some(cc))

  def withFallbacks(fallbacks: Fallbacks): MessagesRequest = copy(fallbacks = Some(fallbacks))

  def beta(beta: String): MessagesRequest = copy(betas = betas :+ beta)

  /** Any other top-level parameter. */
  def param(key: String, value: Json): MessagesRequest = copy(extra = extra + (key -> value))

  /** Checks the rules API version `2026-10-21` added, which the client applies before sending on that
    * version: no `temperature`, `top_p` or `top_k` (they were removed), no `document` block directly in
    * an assistant message (send documents in user messages), and no `citations` on text in a user
    * message, `search_result` blocks included (citations stay accepted on echoed assistant text).
    */
  def validate: Either[AnthropicException.InvalidRequest, Unit] = {
    val removed = Seq("temperature", "top_p", "top_k").find(extra.contains)
    removed match {
      case Some(p) => Left(AnthropicException.InvalidRequest(s"`$p` was removed in API version 2026-10-21"))
      case None    =>
        val problems = messages.iterator.zipWithIndex.flatMap { case (message, i) =>
          val blocks = message match {
            case MessageParam.User(Content.Blocks(b))      => b
            case MessageParam.Assistant(Content.Blocks(b)) => b
            case _                                         => Vector.empty
          }
          blocks.iterator.flatMap { block =>
            if (message.role == Role.Assistant && block.kind.contains("document"))
              Some(s"messages[$i]: send `document` blocks in user messages, not assistant ones")
            else if (message.role == Role.User && block.hasCitations)
              Some(s"messages[$i]: a user message cannot carry `citations` on its text")
            else None
          }
        }
        if (problems.hasNext) Left(AnthropicException.InvalidRequest(problems.next())) else Right(())
    }
  }

  /** The `anthropic-beta` values this request needs: its own, then the ones its `fallbacks` form and task
    * budget require (unless already present), without duplicates.
    */
  def betaHeaderValues: Vector[String] = {
    val fallback = fallbacks.map {
      case Fallbacks.Default   => MessagesRequest.BetaFallbackDefault
      case Fallbacks.Models(_) => MessagesRequest.BetaFallbackList
    }
    val budget = outputConfig.flatMap(_.taskBudget).map(_ => MessagesRequest.BetaTaskBudget)
    (betas ++ fallback ++ budget).distinct
  }

  /** The JSON body, without `stream`. */
  def toJson: Json.Obj = {
    val fields = Vector[Option[(String, Json)]](
      Some("model" -> Json.Str(model)),
      Some("max_tokens" -> Json.num(maxTokens.toLong)),
      Some("messages" -> Json.Arr(messages.map(_.toJson))),
      system.map(s => "system" -> s.toJson),
      thinking.map(t => "thinking" -> t.toJson),
      outputConfig.map(o => "output_config" -> o.toJson),
      if (tools.isEmpty) None else Some("tools" -> Json.Arr(tools.map(_.toJson))),
      toolChoice.map(t => "tool_choice" -> t.toJson),
      if (stopSequences.isEmpty) None else Some("stop_sequences" -> Json.strings(stopSequences)),
      cacheControl.map(c => "cache_control" -> c.toJson),
      fallbacks.map(f => "fallbacks" -> f.toJson)
    ).flatten
    Json.Obj(VectorMap.from(fields) ++ extra)
  }

  /** The body of `POST /v1/messages/count_tokens`: the prompt-shaping fields only. */
  private[anthropic] def countTokensJson: Json.Obj = {
    val fields = Vector[Option[(String, Json)]](
      Some("model" -> Json.Str(model)),
      Some("messages" -> Json.Arr(messages.map(_.toJson))),
      system.map(s => "system" -> s.toJson),
      if (tools.isEmpty) None else Some("tools" -> Json.Arr(tools.map(_.toJson))),
      toolChoice.map(t => "tool_choice" -> t.toJson),
      thinking.map(t => "thinking" -> t.toJson)
    ).flatten
    Json.Obj(VectorMap.from(fields))
  }
}

object MessagesRequest {

  /** Beta header for `fallbacks: "default"`. */
  val BetaFallbackDefault = "server-side-fallback-2026-07-01"

  /** Beta header for the array form `fallbacks: [{"model": ...}]`. */
  val BetaFallbackList = "server-side-fallback-2026-06-01"

  /** Beta header for `output_config.task_budget`. */
  val BetaTaskBudget = "task-budgets-2026-03-13"
}

sealed abstract class Role(val value: String) extends Product with Serializable

object Role {
  case object User      extends Role("user")
  case object Assistant extends Role("assistant")

  /** A mid-conversation operator instruction (see [[MessageParam.system]]). */
  case object System extends Role("system")
}

/** One message of the conversation, typed by its role (sent with its `role`). */
sealed trait MessageParam extends Product with Serializable {
  def role: Role

  /** The content of a user or assistant message; `None` for a system message. */
  def content: Option[Content] = this match {
    case MessageParam.User(c)      => Some(c)
    case MessageParam.Assistant(c) => Some(c)
    case MessageParam.System(_)    => None
  }

  def toJson: Json = this match {
    case MessageParam.User(c)      => Json.obj("role" -> Json.Str("user"), "content" -> c.toJson)
    case MessageParam.Assistant(c) => Json.obj("role" -> Json.Str("assistant"), "content" -> c.toJson)
    case MessageParam.System(c)    => Json.obj("role" -> Json.Str("system"), "content" -> c.toJson)
  }
}

object MessageParam {
  final case class User(body: Content) extends MessageParam { def role: Role = Role.User }

  final case class Assistant(body: Content) extends MessageParam { def role: Role = Role.Assistant }

  /** A mid-conversation operator instruction: text only (see [[MessageParam.system]]). */
  final case class System(body: SystemPrompt) extends MessageParam { def role: Role = Role.System }

  def userText(text: String): MessageParam = User(Content.Text(text))

  def user(blocks: ContentBlockParam*): MessageParam = User(Content.Blocks(blocks.toVector))

  def user(blocks: Iterable[ContentBlockParam]): MessageParam = User(Content.Blocks(blocks.toVector))

  def assistant(blocks: ContentBlockParam*): MessageParam = Assistant(Content.Blocks(blocks.toVector))

  def assistant(blocks: Iterable[ContentBlockParam]): MessageParam = Assistant(Content.Blocks(blocks.toVector))

  /** An assistant turn echoing a response's blocks exactly as received. */
  def assistant(message: Message): MessageParam = assistant(message.content.map(ContentBlockParam.from))

  /** An operator instruction added mid-conversation, instead of editing the top-level `system` (which
    * would invalidate the cached history). It must follow a user turn and be the last message or be
    * followed by an assistant turn; never `messages[0]`. Not every model accepts it (Claude Sonnet 5
    * does not).
    */
  def system(text: String): MessageParam = System(SystemPrompt.Text(text))
}

sealed trait Content extends Product with Serializable {
  def toJson: Json = this match {
    case Content.Text(t)   => Json.Str(t)
    case Content.Blocks(b) => Json.Arr(b.map(_.toJson))
  }
}

object Content {
  final case class Text(text: String)                         extends Content
  final case class Blocks(blocks: Vector[ContentBlockParam]) extends Content
}

/** A block of a request message. */
sealed trait ContentBlockParam extends Product with Serializable {

  /** Enables citations on a document; no effect on other blocks. */
  def withCitations: ContentBlockParam = this match {
    case d: ContentBlockParam.Document => d.copy(citations = Some(CitationsConfig(enabled = true)))
    case other                         => other
  }

  /** Sets the document's non-citable context; no effect on other blocks. */
  def withContext(text: String): ContentBlockParam = this match {
    case d: ContentBlockParam.Document => d.copy(context = Some(text))
    case other                         => other
  }

  /** Marks this block as a cache breakpoint; no effect on blocks that take none. */
  def withCacheControl(cc: CacheControl): ContentBlockParam = this match {
    case b: ContentBlockParam.Text       => b.copy(cacheControl = Some(cc))
    case b: ContentBlockParam.Image      => b.copy(cacheControl = Some(cc))
    case b: ContentBlockParam.Document   => b.copy(cacheControl = Some(cc))
    case b: ContentBlockParam.ToolResult => b.copy(cacheControl = Some(cc))
    case other                           => other
  }

  /** The block's `type`, where the checks need it. */
  private[anthropic] def kind: Option[String] = this match {
    case _: ContentBlockParam.Document => Some("document")
    case ContentBlockParam.Raw(v)      => Decode.kind(v)
    case _                             => None
  }

  /** Whether this block, or a text block inside it (a tool result's content, a search result's
    * content), carries a non-empty `citations` array.
    */
  private[anthropic] def hasCitations: Boolean = this match {
    case ContentBlockParam.Text(_, citations, _)                         => citations.exists(_.nonEmpty)
    case ContentBlockParam.ToolResult(_, ToolResultContent.Blocks(b), _, _) => b.exists(_.hasCitations)
    case ContentBlockParam.Raw(v)                                        => ContentBlockParam.rawHasCitations(v)
    case _                                                               => false
  }

  def toJson: Json = {
    import ContentBlockParam._
    def typed(kind: String, fields: Option[(String, Json)]*): Json =
      Json.Obj(VectorMap.from(("type" -> Json.Str(kind)) +: fields.flatten))
    def cc(c: Option[CacheControl]) = c.map(x => "cache_control" -> x.toJson)
    this match {
      case Text(text, citations, cacheControl) =>
        typed(
          "text",
          Some("text" -> Json.Str(text)),
          citations.map(c => "citations" -> Json.Arr(c.map(_.toJson))),
          cc(cacheControl)
        )
      case Image(source, cacheControl) => typed("image", Some("source" -> source.toJson), cc(cacheControl))
      case Document(source, title, context, citations, cacheControl) =>
        typed(
          "document",
          Some("source" -> source.toJson),
          title.map(t => "title" -> Json.Str(t)),
          context.map(t => "context" -> Json.Str(t)),
          citations.map(c => "citations" -> c.toJson),
          cc(cacheControl)
        )
      case ToolUse(id, name, input) =>
        typed("tool_use", Some("id" -> Json.Str(id)), Some("name" -> Json.Str(name)), Some("input" -> input))
      case ToolResult(toolUseId, content, isError, cacheControl) =>
        typed(
          "tool_result",
          Some("tool_use_id" -> Json.Str(toolUseId)),
          Some("content" -> content.toJson),
          isError.map(e => "is_error" -> Json.Bool(e)),
          cc(cacheControl)
        )
      case Thinking(thinking, signature) =>
        typed("thinking", Some("thinking" -> Json.Str(thinking)), Some("signature" -> Json.Str(signature)))
      case RedactedThinking(data) => typed("redacted_thinking", Some("data" -> Json.Str(data)))
      case Raw(v)                 => v
    }
  }
}

object ContentBlockParam {

  /** @param citations only on echoed assistant text that carried citations */
  final case class Text(text: String, citations: Option[Vector[Citation]] = None, cacheControl: Option[CacheControl] = None)
      extends ContentBlockParam

  final case class Image(source: ImageSource, cacheControl: Option[CacheControl] = None) extends ContentBlockParam

  /** @param context   shown to the model but never cited from
    * @param citations citations must be enabled on all documents of a request or on none
    */
  final case class Document(
      source: DocumentSource,
      title: Option[String] = None,
      context: Option[String] = None,
      citations: Option[CitationsConfig] = None,
      cacheControl: Option[CacheControl] = None
  ) extends ContentBlockParam

  final case class ToolUse(id: String, name: String, input: Json) extends ContentBlockParam

  final case class ToolResult(
      toolUseId: String,
      content: ToolResultContent,
      isError: Option[Boolean] = None,
      cacheControl: Option[CacheControl] = None
  ) extends ContentBlockParam

  /** Echoed back unchanged on the same model. */
  final case class Thinking(thinking: String, signature: String) extends ContentBlockParam

  final case class RedactedThinking(data: String) extends ContentBlockParam

  /** Any other block, sent verbatim (e.g. an echoed block type this SDK does not model). */
  final case class Raw(json: Json) extends ContentBlockParam

  def text(text: String): ContentBlockParam = Text(text)

  /** A plain-text document; the API splits it into sentences for citations. */
  def textDocument(data: String, title: String): ContentBlockParam =
    Document(DocumentSource.Text("text/plain", data), title = Some(title))

  /** A document made of chunks you choose; citations point at chunk indices. */
  def chunkedDocument(chunks: Iterable[String], title: String): ContentBlockParam =
    Document(DocumentSource.Content(chunks.iterator.map(c => Text(c): ContentBlockParam).toVector), title = Some(title))

  /** A base64-encoded PDF. */
  def base64Pdf(data: String): ContentBlockParam = Document(DocumentSource.Base64("application/pdf", data))

  /** A PDF from its bytes. */
  def pdf(bytes: Array[Byte]): ContentBlockParam = base64Pdf(java.util.Base64.getEncoder.encodeToString(bytes))

  /** A document uploaded with the Files API (PDF or plain text). */
  def fileDocument(fileId: String): ContentBlockParam = Document(DocumentSource.File(fileId))

  /** An image uploaded with the Files API. */
  def fileImage(fileId: String): ContentBlockParam = Image(ImageSource.File(fileId))

  /** A base64-encoded image (`image/jpeg`, `image/png`, `image/gif` or `image/webp`). */
  def base64Image(mediaType: String, data: String): ContentBlockParam = Image(ImageSource.Base64(mediaType, data))

  def imageUrl(url: String): ContentBlockParam = Image(ImageSource.Url(url))

  def toolResult(toolUseId: String, content: String, isError: Boolean = false): ContentBlockParam =
    ToolResult(toolUseId, ToolResultContent.Text(content), if (isError) Some(true) else None)

  /** Echoing an assistant turn: every block goes back as received. */
  def from(block: ContentBlock): ContentBlockParam = block match {
    case ContentBlock.Text(text, citations)  => Text(text, if (citations.isEmpty) None else Some(citations))
    case ContentBlock.Thinking(t, signature) => Thinking(t, signature)
    case ContentBlock.RedactedThinking(data) => RedactedThinking(data)
    case ContentBlock.ToolUse(id, name, in)  => ToolUse(id, name, in)
    case other                               => Raw(other.toJson)
  }

  /** A raw `text` block with non-empty `citations`, or a `search_result` or `tool_result` whose content
    * holds one.
    */
  private[anthropic] def rawHasCitations(v: Json): Boolean = Decode.kind(v) match {
    case Some("text") => v.get("citations").flatMap(_.asArray).exists(_.nonEmpty)
    case Some("search_result") | Some("tool_result") =>
      v.get("content").flatMap(_.asArray).exists(_.exists(rawHasCitations))
    case _ => false
  }
}

sealed trait DocumentSource extends Product with Serializable {
  def toJson: Json = this match {
    case DocumentSource.Text(m, d) =>
      Json.obj("type" -> Json.Str("text"), "media_type" -> Json.Str(m), "data" -> Json.Str(d))
    case DocumentSource.Base64(m, d) =>
      Json.obj("type" -> Json.Str("base64"), "media_type" -> Json.Str(m), "data" -> Json.Str(d))
    case DocumentSource.Content(c) => Json.obj("type" -> Json.Str("content"), "content" -> Json.Arr(c.map(_.toJson)))
    case DocumentSource.Url(u)     => Json.obj("type" -> Json.Str("url"), "url" -> Json.Str(u))
    case DocumentSource.File(id)   => Json.obj("type" -> Json.Str("file"), "file_id" -> Json.Str(id))
  }
}

object DocumentSource {
  final case class Text(mediaType: String, data: String)          extends DocumentSource
  final case class Base64(mediaType: String, data: String)        extends DocumentSource
  final case class Content(content: Vector[ContentBlockParam])    extends DocumentSource
  final case class Url(url: String)                               extends DocumentSource
  final case class File(fileId: String)                           extends DocumentSource
}

sealed trait ImageSource extends Product with Serializable {
  def toJson: Json = this match {
    case ImageSource.Base64(m, d) =>
      Json.obj("type" -> Json.Str("base64"), "media_type" -> Json.Str(m), "data" -> Json.Str(d))
    case ImageSource.Url(u)   => Json.obj("type" -> Json.Str("url"), "url" -> Json.Str(u))
    case ImageSource.File(id) => Json.obj("type" -> Json.Str("file"), "file_id" -> Json.Str(id))
  }
}

object ImageSource {
  final case class Base64(mediaType: String, data: String) extends ImageSource
  final case class Url(url: String)                        extends ImageSource
  final case class File(fileId: String)                    extends ImageSource
}

sealed trait ToolResultContent extends Product with Serializable {
  def toJson: Json = this match {
    case ToolResultContent.Text(t)   => Json.Str(t)
    case ToolResultContent.Blocks(b) => Json.Arr(b.map(_.toJson))
  }
}

object ToolResultContent {
  final case class Text(text: String)                         extends ToolResultContent
  final case class Blocks(blocks: Vector[ContentBlockParam]) extends ToolResultContent
}

final case class CitationsConfig(enabled: Boolean) {
  def toJson: Json = Json.obj("enabled" -> Json.Bool(enabled))
}

/** @param ttl `"5m"` (the default) or `"1h"` */
final case class CacheControl(kind: String = "ephemeral", ttl: Option[String] = None) {
  def toJson: Json = Json.Obj(VectorMap.from(Vector("type" -> Json.Str(kind)) ++ ttl.map(t => "ttl" -> Json.Str(t))))
}

object CacheControl {
  val ephemeral: CacheControl = CacheControl()
  val oneHour: CacheControl   = CacheControl(ttl = Some("1h"))
}

/** The top-level `system` prompt, or the content of a mid-conversation system message. */
sealed trait SystemPrompt extends Product with Serializable {
  def toJson: Json = this match {
    case SystemPrompt.Text(t)   => Json.Str(t)
    case SystemPrompt.Blocks(b) => Json.Arr(b.map(_.toJson))
  }
}

object SystemPrompt {
  final case class Text(text: String)                  extends SystemPrompt
  final case class Blocks(blocks: Vector[SystemBlock]) extends SystemPrompt
}

/** A system prompt block; sent with `"type": "text"`. */
final case class SystemBlock(text: String, cacheControl: Option[CacheControl] = None) {
  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector("type" -> Json.Str("text"), "text" -> Json.Str(text)) ++ cacheControl.map(c => "cache_control" -> c.toJson)
      )
    )
}

sealed trait ThinkingConfig extends Product with Serializable {
  def toJson: Json = this match {
    case ThinkingConfig.Adaptive(display) =>
      Json.Obj(
        VectorMap.from(Vector("type" -> Json.Str("adaptive")) ++ display.map(d => "display" -> Json.Str(d.value)))
      )
    case ThinkingConfig.Disabled         => Json.obj("type" -> Json.Str("disabled"))
    case ThinkingConfig.Enabled(budget)  => Json.obj("type" -> Json.Str("enabled"), "budget_tokens" -> Json.num(budget.toLong))
    case ThinkingConfig.BetweenTools     => Json.obj("type" -> Json.Str("between_tools"))
  }
}

object ThinkingConfig {

  /** The model decides when and how much to think; `effort` sets the depth. */
  final case class Adaptive(display: Option[ThinkingDisplay] = None) extends ThinkingConfig

  /** Rejected by several current models (see the model's documentation). */
  case object Disabled extends ThinkingConfig

  /** Older models only. */
  final case class Enabled(budgetTokens: Int) extends ThinkingConfig

  /** Claude Sonnet 5.5's way to turn thinking off. */
  case object BetweenTools extends ThinkingConfig

  val adaptive: ThinkingConfig = Adaptive()
}

sealed abstract class ThinkingDisplay(val value: String) extends Product with Serializable

object ThinkingDisplay {
  case object Summarized extends ThinkingDisplay("summarized")
  case object Omitted    extends ThinkingDisplay("omitted")
  case object Updates    extends ThinkingDisplay("updates")
}

/** @param format     structured output format (cannot be combined with citations)
  * @param taskBudget token budget for an agentic loop (beta; see [[MessagesRequest.withTaskBudget]])
  */
final case class OutputConfig(
    effort: Option[Effort] = None,
    format: Option[Json] = None,
    taskBudget: Option[TaskBudget] = None
) {
  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          effort.map(e => "effort" -> Json.Str(e.value)),
          format.map(f => "format" -> f),
          taskBudget.map(t => "task_budget" -> t.toJson)
        ).flatten
      )
    )
}

/** `output_config.task_budget`.
  *
  * @param remaining what is left of `total`; the server tracks it unless history was rewritten
  */
final case class TaskBudget(total: Int, remaining: Option[Int] = None) {
  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector("type" -> Json.Str("tokens"), "total" -> Json.num(total.toLong)) ++
          remaining.map(r => "remaining" -> Json.num(r.toLong))
      )
    )
}

sealed abstract class Effort(val value: String) extends Product with Serializable

object Effort {
  case object Low    extends Effort("low")
  case object Medium extends Effort("medium")
  case object High   extends Effort("high")
  case object Xhigh  extends Effort("xhigh")
  case object Max    extends Effort("max")
}

/** A tool definition: a custom [[Tool]], a server tool Anthropic runs ([[WebSearchTool]],
  * [[WebFetchTool]], [[CodeExecutionTool]], [[ToolSearchTool]]), or anything else as raw JSON.
  */
sealed trait ToolDefinition extends Product with Serializable {
  def toJson: Json
}

object ToolDefinition {
  final case class Raw(json: Json) extends ToolDefinition {
    def toJson: Json = json
  }
}

/** A tool you run. Its calls come back as [[ContentBlock.ToolUse]] blocks; [[AnthropicClient.runTools]]
  * runs them for you.
  *
  * @param deferLoading        loaded only when a tool search finds it (see [[ToolSearchTool]])
  * @param eagerInputStreaming stream the input as it is generated (see [[withEagerInputStreaming]])
  */
final case class Tool(
    name: String,
    description: String,
    inputSchema: Json,
    strict: Option[Boolean] = None,
    deferLoading: Option[Boolean] = None,
    eagerInputStreaming: Option[Boolean] = None,
    cacheControl: Option[CacheControl] = None
) extends ToolDefinition {

  /** When streaming, large inputs arrive as they are generated instead of in one burst at the end. The
    * API then no longer validates the input: it may be cut short (`max_tokens`) or invalid JSON, so
    * check it before running the tool. Leave it off for requests that are not streamed.
    */
  def withEagerInputStreaming: Tool = copy(eagerInputStreaming = Some(true))

  /** Inputs always validate against the schema (which needs `additionalProperties: false`). */
  def withStrict: Tool = copy(strict = Some(true))

  /** Loaded only when a [[ToolSearchTool]] finds it. */
  def deferred: Tool = copy(deferLoading = Some(true))

  def withCacheControl(cc: CacheControl): Tool = copy(cacheControl = Some(cc))

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("name" -> Json.Str(name)),
          Some("description" -> Json.Str(description)),
          Some("input_schema" -> inputSchema),
          strict.map(b => "strict" -> Json.Bool(b)),
          deferLoading.map(b => "defer_loading" -> Json.Bool(b)),
          eagerInputStreaming.map(b => "eager_input_streaming" -> Json.Bool(b)),
          cacheControl.map(c => "cache_control" -> c.toJson)
        ).flatten
      )
    )
}

/** Server-side web search. Results come back as [[ContentBlock.WebSearchToolResult]] blocks and cited
  * text carries [[Citation.WebSearchResultLocation]]. A failed search is an error object inside the
  * result block, not an API error.
  *
  * The default `kind` is the version with dynamic filtering (Claude Opus 4.6, Claude Sonnet 4.6 and
  * later); do not also declare [[CodeExecutionTool]] with it: it runs code on its own.
  *
  * @param kind           the tool version, e.g. `web_search_20260209`
  * @param allowedDomains use `allowedDomains` or `blockedDomains`, not both
  */
final case class WebSearchTool(
    kind: String = "web_search_20260209",
    name: String = "web_search",
    maxUses: Option[Int] = None,
    allowedDomains: Option[Vector[String]] = None,
    blockedDomains: Option[Vector[String]] = None,
    userLocation: Option[UserLocation] = None,
    cacheControl: Option[CacheControl] = None
) extends ToolDefinition {
  def withMaxUses(n: Int): WebSearchTool                    = copy(maxUses = Some(n))
  def withAllowedDomains(domains: String*): WebSearchTool   = copy(allowedDomains = Some(domains.toVector))
  def withBlockedDomains(domains: String*): WebSearchTool   = copy(blockedDomains = Some(domains.toVector))
  def withUserLocation(location: UserLocation): WebSearchTool = copy(userLocation = Some(location))
  def withCacheControl(cc: CacheControl): WebSearchTool     = copy(cacheControl = Some(cc))

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("type" -> Json.Str(kind)),
          Some("name" -> Json.Str(name)),
          maxUses.map(n => "max_uses" -> Json.num(n.toLong)),
          allowedDomains.map(d => "allowed_domains" -> Json.strings(d)),
          blockedDomains.map(d => "blocked_domains" -> Json.strings(d)),
          userLocation.map(u => "user_location" -> u.toJson),
          cacheControl.map(c => "cache_control" -> c.toJson)
        ).flatten
      )
    )
}

object WebSearchTool {

  /** The basic version, for older models and Vertex AI. */
  def basic: WebSearchTool = WebSearchTool(kind = "web_search_20250305")

  def version(kind: String): WebSearchTool = WebSearchTool(kind = kind)
}

/** Where the user roughly is, to localise search results; sent with `"type": "approximate"`.
  *
  * @param country  ISO 3166-1 alpha-2, e.g. `RO`
  * @param timezone IANA time zone, e.g. `Europe/Bucharest`
  */
final case class UserLocation(
    city: Option[String] = None,
    region: Option[String] = None,
    country: Option[String] = None,
    timezone: Option[String] = None
) {
  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("type" -> Json.Str("approximate")),
          city.map(v => "city" -> Json.Str(v)),
          region.map(v => "region" -> Json.Str(v)),
          country.map(v => "country" -> Json.Str(v)),
          timezone.map(v => "timezone" -> Json.Str(v))
        ).flatten
      )
    )
}

/** Server-side fetch of a URL already present in the conversation. The page comes back as a
  * [[ContentBlock.WebFetchToolResult]] block. The default `kind` is the version with dynamic filtering
  * (Claude Opus 4.6, Claude Sonnet 4.6 and later).
  *
  * @param citations        lets the answer cite the fetched pages
  * @param maxContentTokens truncates long pages to about this many tokens
  */
final case class WebFetchTool(
    kind: String = "web_fetch_20260209",
    name: String = "web_fetch",
    maxUses: Option[Int] = None,
    allowedDomains: Option[Vector[String]] = None,
    blockedDomains: Option[Vector[String]] = None,
    citations: Option[CitationsConfig] = None,
    maxContentTokens: Option[Int] = None,
    cacheControl: Option[CacheControl] = None
) extends ToolDefinition {
  def withMaxUses(n: Int): WebFetchTool                  = copy(maxUses = Some(n))
  def withAllowedDomains(domains: String*): WebFetchTool = copy(allowedDomains = Some(domains.toVector))
  def withBlockedDomains(domains: String*): WebFetchTool = copy(blockedDomains = Some(domains.toVector))
  def withCitations: WebFetchTool                        = copy(citations = Some(CitationsConfig(enabled = true)))
  def withMaxContentTokens(n: Int): WebFetchTool         = copy(maxContentTokens = Some(n))
  def withCacheControl(cc: CacheControl): WebFetchTool   = copy(cacheControl = Some(cc))

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector(
          Some("type" -> Json.Str(kind)),
          Some("name" -> Json.Str(name)),
          maxUses.map(n => "max_uses" -> Json.num(n.toLong)),
          allowedDomains.map(d => "allowed_domains" -> Json.strings(d)),
          blockedDomains.map(d => "blocked_domains" -> Json.strings(d)),
          citations.map(c => "citations" -> c.toJson),
          maxContentTokens.map(n => "max_content_tokens" -> Json.num(n.toLong)),
          cacheControl.map(c => "cache_control" -> c.toJson)
        ).flatten
      )
    )
}

object WebFetchTool {

  /** The basic version, for older models. */
  def basic: WebFetchTool = WebFetchTool(kind = "web_fetch_20250910")

  def version(kind: String): WebFetchTool = WebFetchTool(kind = kind)
}

/** Server-side code execution in a sandbox. Output comes back as
  * [[ContentBlock.BashCodeExecutionToolResult]] and [[ContentBlock.TextEditorCodeExecutionToolResult]]
  * blocks; files it writes can be downloaded with [[AnthropicClient.downloadFile]].
  *
  * @param kind the tool version, e.g. `code_execution_20260521`; `code_execution_20260120` for
  *             programmatic tool calling
  */
final case class CodeExecutionTool(
    kind: String = "code_execution_20260521",
    name: String = "code_execution",
    cacheControl: Option[CacheControl] = None
) extends ToolDefinition {
  def withCacheControl(cc: CacheControl): CodeExecutionTool = copy(cacheControl = Some(cc))

  def toJson: Json =
    Json.Obj(
      VectorMap.from(
        Vector("type" -> Json.Str(kind), "name" -> Json.Str(name)) ++ cacheControl.map(c => "cache_control" -> c.toJson)
      )
    )
}

object CodeExecutionTool {
  def version(kind: String): CodeExecutionTool = CodeExecutionTool(kind = kind)
}

/** Server-side search over the request's deferred tools ([[Tool.deferred]]): only the tools it finds are
  * loaded. Keep at least one tool, this one included, not deferred.
  *
  * @param kind `tool_search_tool_regex_20251119` or `tool_search_tool_bm25_20251119`
  */
final case class ToolSearchTool(kind: String, name: String) extends ToolDefinition {
  def toJson: Json = Json.obj("type" -> Json.Str(kind), "name" -> Json.Str(name))
}

object ToolSearchTool {

  /** The model searches with regular expressions. */
  val regex: ToolSearchTool = ToolSearchTool("tool_search_tool_regex_20251119", "tool_search_tool_regex")

  /** The model searches with natural-language queries. */
  val bm25: ToolSearchTool = ToolSearchTool("tool_search_tool_bm25_20251119", "tool_search_tool_bm25")
}

sealed trait ToolChoice extends Product with Serializable {
  def toJson: Json = this match {
    case ToolChoice.Auto(disable) =>
      Json.Obj(
        VectorMap.from(
          Vector("type" -> Json.Str("auto")) ++ disable.map(d => "disable_parallel_tool_use" -> Json.Bool(d))
        )
      )
    case ToolChoice.NoTools        => Json.obj("type" -> Json.Str("none"))
    case ToolChoice.Any            => Json.obj("type" -> Json.Str("any"))
    case ToolChoice.Specific(name) => Json.obj("type" -> Json.Str("tool"), "name" -> Json.Str(name))
  }
}

object ToolChoice {
  final case class Auto(disableParallelToolUse: Option[Boolean] = None) extends ToolChoice

  /** `{"type": "none"}`: the model may not call tools. */
  case object NoTools extends ToolChoice

  /** Rejected by several current models; prefer `Auto` and say which tool to use. */
  case object Any extends ToolChoice

  /** `{"type": "tool", "name": ...}`. Rejected by several current models; prefer `Auto` and say which
    * tool to use.
    */
  final case class Specific(name: String) extends ToolChoice

  val auto: ToolChoice = Auto()
}

/** Which model the API retries a refused request on. */
sealed trait Fallbacks extends Product with Serializable {
  def toJson: Json = this match {
    case Fallbacks.Default      => Json.Str("default")
    case Fallbacks.Models(list) => Json.Arr(list.map(_.toJson))
  }
}

object Fallbacks {

  /** `"default"`: the API picks the fallback by refusal category. */
  case object Default extends Fallbacks

  /** Named models, tried in order. */
  final case class Models(models: Vector[FallbackModel]) extends Fallbacks

  def models(names: String*): Fallbacks = Models(names.map(FallbackModel(_)).toVector)
}

final case class FallbackModel(model: String, maxTokens: Option[Int] = None) {
  def toJson: Json =
    Json.Obj(
      VectorMap.from(Vector("model" -> Json.Str(model)) ++ maxTokens.map(n => "max_tokens" -> Json.num(n.toLong)))
    )
}
