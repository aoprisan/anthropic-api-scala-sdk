package anthropic.catseffect

import anthropic._
import cats.effect.{Async, Resource}
import cats.syntax.all._
import fs2.{Pipe, Stream}
import scala.concurrent.duration._

/** The `anthropic.AnthropicClient` for Cats Effect: every call is an `F`, cancelable (cancelling it
  * aborts the HTTP exchange and stops the retry loop), and every body read piece by piece is an fs2
  * `Stream` that releases its connection when the stream ends, fails or is cancelled.
  *
  * {{{
  * AnthropicClientF.fromEnv[IO].use { client =>
  *   client
  *     .stream(MessagesRequest("claude-opus-5-5", 1024).user("Salut!"))
  *     .through(AnthropicClientF.textDeltas)
  *     .foreach(t => IO.print(t))
  *     .compile
  *     .drain
  * }
  * }}}
  */
final class AnthropicClientF[F[_]] private (val underlying: AnthropicClient)(implicit F: Async[F]) {

  private def run[A](thunk: => A): F[A] = F.interruptible(thunk)

  // ---- Messages --------------------------------------------------------------------------------

  /** `POST /v1/messages`. */
  def create(request: MessagesRequest, options: CallOptions = CallOptions.default): F[Message] =
    run(underlying.create(request, options))

  /** The events of a streamed message. Opening the stream is retried; an error after the first event
    * fails the stream (an `error` event as an `AnthropicException.ApiError` with no status).
    * Fold it into the message with [[AnthropicClientF.accumulate]], or keep the text with
    * [[AnthropicClientF.textDeltas]].
    */
  def stream(request: MessagesRequest, options: CallOptions = CallOptions.default): Stream[F, StreamEvent] =
    Stream
      .bracket(run(underlying.stream(request, options)))(s => F.delay(s.close()))
      .flatMap(s => Stream.fromBlockingIterator[F](s, 1))

  /** Streams the request and returns the accumulated message, for long outputs or large `max_tokens`. */
  def createStreamed(request: MessagesRequest, options: CallOptions = CallOptions.default): F[Message] =
    run(underlying.createStreamed(request, options))

  /** `POST /v1/messages/count_tokens`. */
  def countTokens(request: MessagesRequest, options: CallOptions = CallOptions.default): F[Long] =
    run(underlying.countTokens(request, options))

  /** The tool-use loop of `anthropic.ToolLoop` with an effectful handler: the calls of one turn run
    * concurrently when `options.parallel` is set, their results go back in call order, and a handler
    * that fails sends its error's message back as an error result. The conversation, grown by every
    * turn, is `ToolRun.request`; run the loop on it again to resume a run cut short.
    */
  def runTools(request: MessagesRequest, options: ToolLoopOptions = ToolLoopOptions())(
      handler: ToolCall => F[ToolOutput]
  ): F[ToolRun] = {
    def runCalls(calls: Vector[ToolCall]): F[Vector[ContentBlockParam]] = {
      def one(call: ToolCall): F[ContentBlockParam] =
        F.defer(handler(call))
          .handleError(e => ToolOutput.error(ToolOutput.errorText(e)))
          .map(_.toBlock(call.id))
      if (options.parallel && calls.length > 1) F.parTraverseN(calls.length)(calls)(one) else calls.traverse(one)
    }
    def withResults(req: MessagesRequest, calls: Vector[ToolCall]): F[MessagesRequest] =
      runCalls(calls).map(results => req.message(MessageParam.User(Content.Blocks(results))))

    def loop(req: MessagesRequest, requests: Int, usage: Usage): F[ToolRun] = {
      val send = if (options.stream) createStreamed(req, options.call) else create(req, options.call)
      send.flatMap { message =>
        val n             = requests + 1
        val total         = usage + message.usage
        val (next, calls) = ToolLoop.afterResponse(req, message)
        if (!ToolLoop.continues(message, calls) || n >= math.max(options.maxRequests, 1))
          F.pure(ToolRun(message, n, total, next))
        else if (message.stopReason.contains(StopReason.ToolUse)) withResults(next, calls).flatMap(loop(_, n, total))
        else loop(next, n, total)
      }
    }

    val pending = request.messages.lastOption.map(ToolLoop.unansweredCalls).getOrElse(Vector.empty)
    val start   = if (pending.isEmpty) F.pure(request) else withResults(request, pending)
    start.flatMap(loop(_, 0, Usage()))
  }

  // ---- Message Batches -------------------------------------------------------------------------

  def createBatch(requests: Seq[BatchRequest], options: CallOptions = CallOptions.default): F[MessageBatch] =
    run(underlying.createBatch(requests, options))

  def batch(id: String, options: CallOptions = CallOptions.default): F[MessageBatch] = run(underlying.batch(id, options))

  /** Polls the batch every `pollEvery` (sleeping on the runtime, not on a thread) until it has ended. */
  def awaitBatch(id: String, pollEvery: FiniteDuration = 30.seconds): F[MessageBatch] =
    batch(id).flatMap(b => if (b.isEnded) F.pure(b) else F.sleep(pollEvery) >> awaitBatch(id, pollEvery))

  def listBatches(params: ListParams = ListParams(), options: CallOptions = CallOptions.default): F[Page[MessageBatch]] =
    run(underlying.listBatches(params, options))

  /** Every batch, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allBatches(pageSize: Int = 100): Stream[F, MessageBatch] = AnthropicClientF.pages(pageSize)(listBatches(_))

  def cancelBatch(id: String, options: CallOptions = CallOptions.default): F[MessageBatch] =
    run(underlying.cancelBatch(id, options))

  def deleteBatch(id: String, options: CallOptions = CallOptions.default): F[Deleted] =
    run(underlying.deleteBatch(id, options))

  /** The results of an ended batch, decoded line by line as they download. */
  def batchResults(id: String, options: CallOptions = CallOptions.default): Stream[F, BatchResult] =
    Stream
      .bracket(run(underlying.batchResults(id, options)))(r => F.delay(r.close()))
      .flatMap(r => Stream.fromBlockingIterator[F](r, 16))

  // ---- Files -----------------------------------------------------------------------------------

  def uploadFile(filename: String, mimeType: String, data: Array[Byte], options: CallOptions = CallOptions.default)
      : F[FileMetadata] =
    run(underlying.uploadFile(filename, mimeType, data, options))

  def listFiles(params: ListParams = ListParams(), options: CallOptions = CallOptions.default): F[Page[FileMetadata]] =
    run(underlying.listFiles(params, options))

  /** Every file, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allFiles(pageSize: Int = 100): Stream[F, FileMetadata] = AnthropicClientF.pages(pageSize)(listFiles(_))

  def file(id: String, options: CallOptions = CallOptions.default): F[FileMetadata] = run(underlying.file(id, options))

  def downloadFile(id: String, options: CallOptions = CallOptions.default): F[Array[Byte]] =
    run(underlying.downloadFile(id, options))

  def deleteFile(id: String, options: CallOptions = CallOptions.default): F[Deleted] =
    run(underlying.deleteFile(id, options))

  // ---- Models ----------------------------------------------------------------------------------

  def listModels(params: ListParams = ListParams(), options: CallOptions = CallOptions.default): F[Page[Model]] =
    run(underlying.listModels(params, options))

  /** Every model, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allModels(pageSize: Int = 100): Stream[F, Model] = AnthropicClientF.pages(pageSize)(listModels(_))

  def model(id: String, options: CallOptions = CallOptions.default): F[Model] = run(underlying.model(id, options))

  override def toString: String = s"AnthropicClientF(${underlying.config})"
}

object AnthropicClientF {

  /** A client released with the resource. */
  def apply[F[_]](config: ClientConfig)(implicit F: Async[F]): Resource[F, AnthropicClientF[F]] =
    Resource.fromAutoCloseable(F.delay(AnthropicClient(config))).map(new AnthropicClientF[F](_))

  /** A client configured from `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` / `ANTHROPIC_BASE_URL`; the
    * resource fails without a credential.
    */
  def fromEnv[F[_]](implicit F: Async[F]): Resource[F, AnthropicClientF[F]] =
    Resource.fromAutoCloseable(F.delay(AnthropicClient.fromEnv())).map(new AnthropicClientF[F](_))

  /** Wraps a client whose lifetime is managed elsewhere. */
  def wrap[F[_]](client: AnthropicClient)(implicit F: Async[F]): AnthropicClientF[F] = new AnthropicClientF[F](client)

  /** Every item of a list endpoint, page after page. */
  def pages[F[_], A](pageSize: Int)(fetch: ListParams => F[Page[A]])(implicit F: Async[F]): Stream[F, A] =
    Stream
      .unfoldLoopEval(ListParams.limit(pageSize))(p => fetch(p).map(page => (page.data, page.nextParams(p))))
      .flatMap(Stream.emits(_))

  /** The text of a streamed message, piece by piece. */
  def textDeltas[F[_]]: Pipe[F, StreamEvent, String] =
    _.collect { case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(t)) => t }

  /** The message as accumulated after each event (from `message_start` on): the last one is the final
    * message. `stream.through(accumulate).compile.lastOrError` gives it.
    */
  def accumulate[F[_]]: Pipe[F, StreamEvent, Message] =
    events =>
      Stream.suspend {
        val acc = new Accumulator
        events.map { e => acc(e); acc.message }.unNone
      }
}
