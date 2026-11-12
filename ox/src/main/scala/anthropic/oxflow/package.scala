package anthropic

import _root_.ox.mapPar
import _root_.ox.flow.Flow
import scala.util.control.NonFatal

/** The `anthropic.AnthropicClient` for Ox. The client is already direct style, and its calls end when
  * their thread is interrupted, so they work as they are inside `supervised`, `par`, `race` and
  * `timeout`. This package adds what Ox has and the JDK does not: `import anthropic.oxflow.*` gives the
  * client its bodies as `Flow`s, which release their connection when the flow completes, fails or is
  * interrupted, and a tool loop whose handlers run in forks of one scope.
  *
  * {{{
  * import anthropic.*
  * import anthropic.oxflow.*
  *
  * supervised:
  *   val client = useCloseableInScope(AnthropicClient.fromEnv())
  *   client.streamFlow(MessagesRequest("claude-opus-5-5", 1024).user("Salut!")).textDeltas.runForeach(print)
  * }}}
  */
package object oxflow {

  extension (client: AnthropicClient) {

    /** The events of a streamed message, requested each time the flow is run. Opening the stream is
      * retried; an error after the first event fails the flow (an `error` event as an
      * `AnthropicException.ApiError` with no status). Fold it into the message with [[accumulate]], or
      * keep the text with [[textDeltas]].
      */
    def streamFlow(request: MessagesRequest, options: CallOptions = CallOptions.default): Flow[StreamEvent] =
      iterate(client.stream(request, options))

    /** The results of an ended batch, decoded line by line as they download. */
    def batchResultsFlow(id: String, options: CallOptions = CallOptions.default): Flow[BatchResult] =
      iterate(client.batchResults(id, options))

    /** Every batch, newest first, fetching pages of `pageSize` as the flow is consumed. */
    def allBatches(pageSize: Int = 100): Flow[MessageBatch] = pages(pageSize)(client.listBatches(_))

    /** Every file, newest first, fetching pages of `pageSize` as the flow is consumed. */
    def allFiles(pageSize: Int = 100): Flow[FileMetadata] = pages(pageSize)(client.listFiles(_))

    /** Every model, newest first, fetching pages of `pageSize` as the flow is consumed. */
    def allModels(pageSize: Int = 100): Flow[Model] = pages(pageSize)(client.listModels(_))

    /** The tool-use loop of `anthropic.ToolLoop` with structured concurrency: the calls of one turn run
      * in forks of one scope when `options.parallel` is set, so interrupting the loop interrupts the
      * handlers still running, and none outlives it. Results go back in call order, and a handler that
      * throws sends its error's message back as an error result. The conversation, grown by every turn,
      * is `ToolRun.request`; run the loop on it again to resume a run cut short.
      */
    def runToolsScoped(request: MessagesRequest, options: ToolLoopOptions = ToolLoopOptions())(
        handler: ToolCall => ToolOutput
    ): ToolRun = {
      def one(call: ToolCall): ContentBlockParam = {
        val out =
          try handler(call)
          catch { case NonFatal(e) => ToolOutput.error(ToolOutput.errorText(e)) }
        out.toBlock(call.id)
      }
      def withResults(req: MessagesRequest, calls: Vector[ToolCall]): MessagesRequest = {
        val results = if (options.parallel && calls.length > 1) calls.mapPar(calls.length)(one) else calls.map(one)
        req.message(MessageParam.User(Content.Blocks(results)))
      }
      @annotation.tailrec
      def loop(req: MessagesRequest, requests: Int, usage: Usage): ToolRun = {
        val message =
          if (options.stream) client.createStreamed(req, options.call) else client.create(req, options.call)
        val n             = requests + 1
        val total         = usage + message.usage
        val (next, calls) = ToolLoop.afterResponse(req, message)
        if (!ToolLoop.continues(message, calls) || n >= math.max(options.maxRequests, 1)) ToolRun(message, n, total, next)
        else if (message.stopReason.contains(StopReason.ToolUse)) loop(withResults(next, calls), n, total)
        else loop(next, n, total)
      }
      val pending = request.messages.lastOption.map(ToolLoop.unansweredCalls).getOrElse(Vector.empty)
      loop(if (pending.isEmpty) request else withResults(request, pending), 0, Usage())
    }
  }

  extension (events: Flow[StreamEvent]) {

    /** The text of a streamed message, piece by piece. */
    def textDeltas: Flow[String] =
      events.collect { case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(t)) => t }

    /** The message as accumulated after each event (from `message_start` on): the last one is the final
      * message. `events.accumulate.runLast()` gives it.
      */
    def accumulate: Flow[Message] =
      Flow.usingEmit { emit =>
        val acc = new Accumulator
        events.runForeach { e =>
          acc(e)
          acc.message.foreach(emit.apply)
        }
      }
  }

  /** Every item of a list endpoint, page after page. */
  def pages[A](pageSize: Int)(fetch: ListParams => Page[A]): Flow[A] =
    Flow.usingEmit { emit =>
      var params = Option(ListParams.limit(pageSize))
      while (params.isDefined) {
        val current = params.get
        val page    = fetch(current)
        page.data.foreach(emit.apply)
        params = page.nextParams(current)
      }
    }

  /** The items of a blocking iterator opened when the flow is run, and closed however it ends. */
  private def iterate[A](open: => Iterator[A] & AutoCloseable): Flow[A] =
    Flow.usingEmit { emit =>
      val it = open
      try it.foreach(emit.apply)
      finally it.close()
    }
}
