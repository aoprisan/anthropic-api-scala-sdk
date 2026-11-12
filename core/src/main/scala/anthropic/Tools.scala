package anthropic

import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

/** One tool call the model asked for. Read its arguments from `input`, e.g. `call.input("city").asString`. */
final case class ToolCall(id: String, name: String, input: Json)

/** What a tool returns to the model. A failure goes back with `isError` set, so the model can try another
  * way; it does not end the loop.
  */
final case class ToolOutput(content: ToolResultContent, isError: Boolean = false) {
  private[anthropic] def toBlock(toolUseId: String): ContentBlockParam =
    ContentBlockParam.ToolResult(toolUseId, content, if (isError) Some(true) else None)
}

object ToolOutput {
  def text(text: String): ToolOutput = ToolOutput(ToolResultContent.Text(text))

  def error(message: String): ToolOutput = ToolOutput(ToolResultContent.Text(message), isError = true)

  /** Text, images or documents. */
  def blocks(blocks: ContentBlockParam*): ToolOutput = ToolOutput(ToolResultContent.Blocks(blocks.toVector))

  /** A `Left` goes back as an error result carrying its message. */
  def fromEither(result: Either[String, String]): ToolOutput = result.fold(error, text)

  /** A failed `Try` goes back as an error result carrying the exception's message. */
  def fromTry(result: scala.util.Try[String]): ToolOutput = result.fold(e => error(errorText(e)), text)

  private[anthropic] def errorText(e: Throwable): String = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
}

/** @param maxRequests most requests one call of the loop sends, `pause_turn` continuations included
  * @param call        applied to every request; a deadline bounds the whole loop
  * @param stream      send each request streamed and accumulate it, for long outputs or large `max_tokens`
  * @param parallel    run the calls of one turn concurrently (on `ExecutionContext.global`, marked
  *                    `blocking`); off, they run one after the other on the calling thread
  */
final case class ToolLoopOptions(
    maxRequests: Int = 20,
    call: CallOptions = CallOptions.default,
    stream: Boolean = false,
    parallel: Boolean = true
)

/** How a tool loop ended.
  *
  * @param message  the last response; its turn is also the last message of `request`
  * @param requests how many requests the loop sent
  * @param usage    token counts summed over the requests; `extra` is left empty
  * @param request  the conversation so far: the original request with every turn of the loop appended
  */
final case class ToolRun(message: Message, requests: Int, usage: Usage, request: MessagesRequest) {

  /** False when the loop stopped at `maxRequests` with tool calls still to run or a paused turn to
    * resume; running the loop again on `request` carries on.
    */
  def isFinished: Boolean = !message.stopReason.exists(r => r == StopReason.ToolUse || r == StopReason.PauseTurn)
}

/** The tool-use loop: send the request, run the tools the model asks for, send their results back, and
  * repeat until the model ends its turn.
  *
  * Each response is appended as an assistant turn. On `tool_use`, the handler runs every call of the turn
  * and the results go back in one user turn, in call order. On `pause_turn` the request is sent again as
  * is, so the server resumes its own tool loop. Any other stop reason (`end_turn`, `max_tokens`,
  * `refusal`...) ends the loop: check it on the returned message.
  *
  * The returned [[ToolRun.request]] stays valid to send whatever happens: when `maxRequests` cut the loop
  * short, running the loop on it again resumes, first running the calls of a trailing assistant turn
  * that has no results yet. To ask something new after a finished run, add a user turn to it. A handler
  * that throws sends the exception's message back as an error result.
  */
object ToolLoop {

  private[anthropic] def run(
      client: AnthropicClient,
      start: MessagesRequest,
      options: ToolLoopOptions,
      handler: ToolCall => ToolOutput
  ): ToolRun = {
    var request = resumePending(start)(calls => runCalls(calls, handler, options.parallel))
    var requests = 0
    var usage    = Usage()
    while (true) {
      val message =
        if (options.stream) client.stream(request, options.call).finalMessage()
        else client.create(request, options.call)
      requests += 1
      usage = usage + message.usage
      val (next, calls) = afterResponse(request, message)
      request = next
      if (!continues(message, calls) || requests >= math.max(options.maxRequests, 1))
        return ToolRun(message, requests, usage, request)
      if (message.stopReason.contains(StopReason.ToolUse))
        request = request.message(MessageParam.User(Content.Blocks(runCalls(calls, handler, options.parallel))))
    }
    throw new IllegalStateException("unreachable")
  }

  /** The request with the results of a trailing assistant turn's unanswered calls appended, if it has any. */
  private[anthropic] def resumePending(request: MessagesRequest)(
      results: Vector[ToolCall] => Vector[ContentBlockParam]
  ): MessagesRequest = {
    val pending = request.messages.lastOption.map(unansweredCalls).getOrElse(Vector.empty)
    if (pending.isEmpty) request else request.message(MessageParam.User(Content.Blocks(results(pending))))
  }

  /** Appends the response as an assistant turn; the tool calls it asks for. */
  private[anthropic] def afterResponse(request: MessagesRequest, message: Message): (MessagesRequest, Vector[ToolCall]) =
    (request.message(MessageParam.assistant(message)), message.toolUses)

  /** Whether the loop goes on after this response. */
  private[anthropic] def continues(message: Message, calls: Vector[ToolCall]): Boolean = message.stopReason match {
    case Some(StopReason.ToolUse)   => calls.nonEmpty
    case Some(StopReason.PauseTurn) => true
    case _                          => false
  }

  /** The tool calls of an assistant turn, which then still need their results. */
  private[anthropic] def unansweredCalls(turn: MessageParam): Vector[ToolCall] = turn match {
    case MessageParam.Assistant(Content.Blocks(blocks)) =>
      blocks.collect { case ContentBlockParam.ToolUse(id, name, input) => ToolCall(id, name, input) }
    case _ => Vector.empty
  }

  /** Runs one turn's calls; results in call order. */
  private def runCalls(calls: Vector[ToolCall], handler: ToolCall => ToolOutput, parallel: Boolean): Vector[ContentBlockParam] = {
    def one(call: ToolCall): ContentBlockParam = {
      val out =
        try handler(call)
        catch { case NonFatal(e) => ToolOutput.error(ToolOutput.errorText(e)) }
      out.toBlock(call.id)
    }
    if (!parallel || calls.length <= 1) calls.map(one)
    else {
      implicit val ec: ExecutionContext = ExecutionContext.global
      Await.result(Future.traverse(calls)(c => Future(blocking(one(c)))), Duration.Inf)
    }
  }
}
