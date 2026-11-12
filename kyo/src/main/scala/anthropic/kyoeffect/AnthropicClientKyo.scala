package anthropic.kyoeffect

import _root_.kyo.*
import anthropic.*
import java.util.concurrent.{ExecutorService, Executors}

/** The `anthropic.AnthropicClient` for Kyo: every call is pending `Async & Abort[AnthropicException]`
  * (the SDK's errors are typed failures, anything else is a panic) and is interruptible (interrupting it
  * aborts the HTTP exchange and stops the retry loop), and every body read piece by piece is a `Stream`
  * that also needs a `Scope`, which releases its connection when it closes.
  *
  * Calls block a virtual thread each, never a thread of Kyo's scheduler.
  *
  * {{{
  * object Hello extends KyoApp:
  *   run {
  *     AnthropicClientKyo.fromEnv.map { client =>
  *       client
  *         .stream(MessagesRequest("claude-opus-5-5", 1024).user("Salut!"))
  *         .textDeltas
  *         .foreach(Console.print(_))
  *     }
  *   }
  * }}}
  */
final class AnthropicClientKyo private (val underlying: AnthropicClient) {
  import AnthropicClientKyo.{iterate, run, Call}

  // ---- Messages --------------------------------------------------------------------------------

  /** `POST /v1/messages`. */
  def create(request: MessagesRequest, options: CallOptions = CallOptions.default)(using Frame): Message < Call =
    run(underlying.create(request, options))

  /** The events of a streamed message. Opening the stream is retried; an error after the first event
    * fails the stream (an `error` event as an `AnthropicException.ApiError` with no status).
    * Fold it into the message with `accumulate`, or keep the text with `textDeltas`.
    */
  def stream(request: MessagesRequest, options: CallOptions = CallOptions.default)(using
      Frame
  ): Stream[StreamEvent, Scope & Call] =
    iterate(run(underlying.stream(request, options)), 1)

  /** Streams the request and returns the accumulated message, for long outputs or large `max_tokens`. */
  def createStreamed(request: MessagesRequest, options: CallOptions = CallOptions.default)(using
      Frame
  ): Message < Call =
    run(underlying.createStreamed(request, options))

  /** `POST /v1/messages/count_tokens`. */
  def countTokens(request: MessagesRequest, options: CallOptions = CallOptions.default)(using Frame): Long < Call =
    run(underlying.countTokens(request, options))

  /** The tool-use loop of `anthropic.ToolLoop` with an effectful handler: the calls of one turn run
    * concurrently when `options.parallel` is set, their results go back in call order, and a handler
    * that fails or panics sends its error's message back as an error result. The conversation, grown by
    * every turn, is `ToolRun.request`; run the loop on it again to resume a run cut short.
    */
  def runTools(request: MessagesRequest, options: ToolLoopOptions = ToolLoopOptions())(
      handler: ToolCall => ToolOutput < (Async & Abort[Throwable])
  )(using Frame): ToolRun < Call = {
    def one(call: ToolCall): ContentBlockParam < Async =
      Abort
        .run[Throwable](Sync.defer(handler(call)))
        .map(_.fold(identity, failure, failure).toBlock(call.id))
    def failure(e: Throwable): ToolOutput = e match {
      case e: Interrupted => throw e
      case e              => ToolOutput.error(ToolOutput.errorText(e))
    }
    def withResults(req: MessagesRequest, calls: Vector[ToolCall]): MessagesRequest < Async = {
      val results =
        if (options.parallel && calls.length > 1) Async.foreach(calls, calls.length)(one) else Kyo.foreach(calls)(one)
      results.map(blocks => req.message(MessageParam.User(Content.Blocks(blocks.toVector))))
    }
    def loop(req: MessagesRequest, requests: Int, usage: Usage): ToolRun < Call = {
      val send = if (options.stream) createStreamed(req, options.call) else create(req, options.call)
      send.map { message =>
        val n             = requests + 1
        val total         = usage + message.usage
        val (next, calls) = ToolLoop.afterResponse(req, message)
        if (!ToolLoop.continues(message, calls) || n >= math.max(options.maxRequests, 1))
          ToolRun(message, n, total, next)
        else if (message.stopReason.contains(StopReason.ToolUse)) withResults(next, calls).map(loop(_, n, total))
        else loop(next, n, total)
      }
    }
    Sync.defer {
      val pending = request.messages.lastOption.map(ToolLoop.unansweredCalls).getOrElse(Vector.empty)
      val start: MessagesRequest < Async = if (pending.isEmpty) request else withResults(request, pending)
      start.map(loop(_, 0, Usage()))
    }
  }

  // ---- Message Batches -------------------------------------------------------------------------

  def createBatch(requests: Seq[BatchRequest], options: CallOptions = CallOptions.default)(using
      Frame
  ): MessageBatch < Call =
    run(underlying.createBatch(requests, options))

  def batch(id: String, options: CallOptions = CallOptions.default)(using Frame): MessageBatch < Call =
    run(underlying.batch(id, options))

  /** Polls the batch every `pollEvery` (sleeping on the scheduler, not on a thread) until it has ended. */
  def awaitBatch(id: String, pollEvery: Duration = 30.seconds)(using Frame): MessageBatch < Call =
    batch(id).map(b => if (b.isEnded) b else Async.sleep(pollEvery).andThen(awaitBatch(id, pollEvery)))

  def listBatches(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)(using
      Frame
  ): Page[MessageBatch] < Call =
    run(underlying.listBatches(params, options))

  /** Every batch, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allBatches(pageSize: Int = 100)(using Frame): Stream[MessageBatch, Call] =
    AnthropicClientKyo.pages(pageSize)(listBatches(_))

  def cancelBatch(id: String, options: CallOptions = CallOptions.default)(using Frame): MessageBatch < Call =
    run(underlying.cancelBatch(id, options))

  def deleteBatch(id: String, options: CallOptions = CallOptions.default)(using Frame): Deleted < Call =
    run(underlying.deleteBatch(id, options))

  /** The results of an ended batch, decoded line by line as they download. */
  def batchResults(id: String, options: CallOptions = CallOptions.default)(using
      Frame
  ): Stream[BatchResult, Scope & Call] =
    iterate(run(underlying.batchResults(id, options)), 16)

  // ---- Files -----------------------------------------------------------------------------------

  def uploadFile(filename: String, mimeType: String, data: Array[Byte], options: CallOptions = CallOptions.default)(
      using Frame
  ): FileMetadata < Call =
    run(underlying.uploadFile(filename, mimeType, data, options))

  def listFiles(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)(using
      Frame
  ): Page[FileMetadata] < Call =
    run(underlying.listFiles(params, options))

  /** Every file, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allFiles(pageSize: Int = 100)(using Frame): Stream[FileMetadata, Call] =
    AnthropicClientKyo.pages(pageSize)(listFiles(_))

  def file(id: String, options: CallOptions = CallOptions.default)(using Frame): FileMetadata < Call =
    run(underlying.file(id, options))

  def downloadFile(id: String, options: CallOptions = CallOptions.default)(using Frame): Array[Byte] < Call =
    run(underlying.downloadFile(id, options))

  def deleteFile(id: String, options: CallOptions = CallOptions.default)(using Frame): Deleted < Call =
    run(underlying.deleteFile(id, options))

  // ---- Models ----------------------------------------------------------------------------------

  def listModels(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)(using
      Frame
  ): Page[Model] < Call =
    run(underlying.listModels(params, options))

  /** Every model, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allModels(pageSize: Int = 100)(using Frame): Stream[Model, Call] =
    AnthropicClientKyo.pages(pageSize)(listModels(_))

  def model(id: String, options: CallOptions = CallOptions.default)(using Frame): Model < Call =
    run(underlying.model(id, options))

  override def toString: String = s"AnthropicClientKyo(${underlying.config})"
}

object AnthropicClientKyo {

  /** What every call needs: fibers, and the SDK's errors as typed failures. */
  type Call = Async & Abort[AnthropicException]

  /** A client released when the scope closes. */
  def init(config: ClientConfig)(using Frame): AnthropicClientKyo < (Scope & Sync & Abort[AnthropicException]) =
    Scope.acquire(Abort.catching[AnthropicException](AnthropicClient(config))).map(new AnthropicClientKyo(_))

  /** A client configured from `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` / `ANTHROPIC_BASE_URL`,
    * released when the scope closes; fails with a `ConfigError` without a credential.
    */
  def fromEnv(using Frame): AnthropicClientKyo < (Scope & Sync & Abort[AnthropicException]) =
    Scope.acquire(Abort.catching[AnthropicException](AnthropicClient.fromEnv())).map(new AnthropicClientKyo(_))

  /** A layer with a client for `config`. */
  def layer(config: ClientConfig)(using Frame): Layer[AnthropicClientKyo, Scope & Sync & Abort[AnthropicException]] =
    Layer(init(config))

  /** A layer with a client configured from the environment (see [[fromEnv]]). */
  def live(using Frame): Layer[AnthropicClientKyo, Scope & Sync & Abort[AnthropicException]] = Layer(fromEnv)

  /** Wraps a client whose lifetime is managed elsewhere. */
  def wrap(client: AnthropicClient): AnthropicClientKyo = new AnthropicClientKyo(client)

  /** Every item of a list endpoint, page after page, a page a chunk. */
  def pages[A, S](pageSize: Int)(fetch: ListParams => Page[A] < S)(using Tag[Emit[Chunk[A]]], Frame): Stream[A, S] =
    Stream[A, S] {
      Loop(Maybe(ListParams.limit(pageSize))) {
        case Present(p) =>
          fetch(p).map(page => Emit.valueWith(Chunk.from(page.data))(Loop.continue(Maybe.fromOption(page.nextParams(p)))))
        case Absent => Loop.done
      }
    }

  extension [S](events: Stream[StreamEvent, S]) {

    /** The text of a streamed message, piece by piece. */
    def textDeltas(using Frame): Stream[String, S] =
      events.collectPure {
        case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(t)) => Present(t)
        case _                                                    => Absent
      }

    /** The message as accumulated after each event (from `message_start` on): the last one is the final
      * message.
      */
    def accumulate(using Frame): Stream[Message, S & Sync] =
      Stream.unwrap(Sync.defer {
        val acc = new Accumulator
        events.collectPure { e => acc(e); Maybe.fromOption(acc.message) }
      })
  }

  // ---- blocking calls --------------------------------------------------------------------------

  private lazy val blocking: ExecutorService =
    Executors.newThreadPerTaskExecutor(Thread.ofVirtual().name("anthropic-sdk-kyo-", 0).factory())

  /** `thunk` on a virtual thread, interrupted when the computation is. */
  private def run[A](thunk: => A)(using Frame): A < Call =
    Sync.Unsafe.defer {
      val promise = Promise.Unsafe.init[A, Abort[AnthropicException]]()
      val work: Runnable = () =>
        promise.completeDiscard(
          try Result.succeed(thunk)
          catch {
            case e: AnthropicException => Result.fail(e)
            case e: Throwable          => Result.panic(e)
          }
        )
      val future = blocking.submit(work)
      promise.onInterrupt { _ => future.cancel(true); () }
      promise.safe.get
    }

  /** The items of a blocking iterator, read `chunk` at a time, closed with the scope. */
  private def iterate[A](open: (Iterator[A] & AutoCloseable) < Call, chunk: Int)(using
      Tag[Emit[Chunk[A]]],
      Frame
  ): Stream[A, Scope & Call] =
    Stream.unwrap(Scope.acquireRelease(open)(it => Sync.defer(it.close())).map { it =>
      Stream.repeatPresent[A, Call](
        run {
          val items = Chunk.newBuilder[A]
          var n     = 0
          while (n < chunk && it.hasNext) { items += it.next(); n += 1 }
          val read = items.result()
          if (read.isEmpty) Absent else Present(read)
        },
        chunk
      )
    })
}
