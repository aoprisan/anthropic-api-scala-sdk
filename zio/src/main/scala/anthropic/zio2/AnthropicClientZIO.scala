package anthropic.zio2

import _root_.zio.{Chunk, Duration => ZDuration, IO, Scope, ZIO, ZLayer}
import _root_.zio.stream.{ZPipeline, ZStream}
import anthropic._
import scala.concurrent.duration._

/** The `anthropic.AnthropicClient` for ZIO 2: every call is a `ZIO` that fails with an
  * `anthropic.AnthropicException` in its typed error channel and is interruptible (interrupting it
  * aborts the HTTP exchange and stops the retry loop), and every body read piece by piece is a
  * `ZStream` that releases its connection when the stream ends, fails or is interrupted.
  *
  * {{{
  * val program = ZIO.serviceWithZIO[AnthropicClientZIO] { client =>
  *   client
  *     .stream(MessagesRequest("claude-opus-5-5", 1024).user("Salut!"))
  *     .via(AnthropicClientZIO.textDeltas)
  *     .runForeach(t => Console.print(t))
  * }
  * program.provide(AnthropicClientZIO.live)
  * }}}
  */
final class AnthropicClientZIO private (val underlying: AnthropicClient) {
  import AnthropicClientZIO.{iterate, run}

  // ---- Messages --------------------------------------------------------------------------------

  /** `POST /v1/messages`. */
  def create(request: MessagesRequest, options: CallOptions = CallOptions.default): IO[AnthropicException, Message] =
    run(underlying.create(request, options))

  /** The events of a streamed message. Opening the stream is retried; an error after the first event
    * fails the stream (an `error` event as an `AnthropicException.ApiError` with no status).
    * Fold it into the message with [[AnthropicClientZIO.accumulate]], or keep the text with
    * [[AnthropicClientZIO.textDeltas]].
    */
  def stream(request: MessagesRequest, options: CallOptions = CallOptions.default)
      : ZStream[Any, AnthropicException, StreamEvent] =
    iterate(run(underlying.stream(request, options)), 1)

  /** Streams the request and returns the accumulated message, for long outputs or large `max_tokens`. */
  def createStreamed(request: MessagesRequest, options: CallOptions = CallOptions.default)
      : IO[AnthropicException, Message] =
    run(underlying.createStreamed(request, options))

  /** `POST /v1/messages/count_tokens`. */
  def countTokens(request: MessagesRequest, options: CallOptions = CallOptions.default): IO[AnthropicException, Long] =
    run(underlying.countTokens(request, options))

  /** The tool-use loop of `anthropic.ToolLoop` with a `ZIO` handler: the calls of one turn run
    * concurrently when `options.parallel` is set, their results go back in call order, and a handler
    * that fails sends its error's message back as an error result. The conversation, grown by every
    * turn, is `ToolRun.request`; run the loop on it again to resume a run cut short.
    */
  def runTools[R](request: MessagesRequest, options: ToolLoopOptions = ToolLoopOptions())(
      handler: ToolCall => ZIO[R, Throwable, ToolOutput]
  ): ZIO[R, AnthropicException, ToolRun] = {
    def runCalls(calls: Vector[ToolCall]): ZIO[R, Nothing, Vector[ContentBlockParam]] = {
      def one(call: ToolCall): ZIO[R, Nothing, ContentBlockParam] =
        ZIO
          .suspend(handler(call))
          .catchAll(e => ZIO.succeed(ToolOutput.error(ToolOutput.errorText(e))))
          .map(_.toBlock(call.id))
      if (options.parallel && calls.length > 1) ZIO.foreachPar(calls)(one) else ZIO.foreach(calls)(one)
    }
    def withResults(req: MessagesRequest, calls: Vector[ToolCall]): ZIO[R, Nothing, MessagesRequest] =
      runCalls(calls).map(results => req.message(MessageParam.User(Content.Blocks(results))))

    def loop(req: MessagesRequest, requests: Int, usage: Usage): ZIO[R, AnthropicException, ToolRun] = {
      val send = if (options.stream) createStreamed(req, options.call) else create(req, options.call)
      send.flatMap { message =>
        val n             = requests + 1
        val total         = usage + message.usage
        val (next, calls) = ToolLoop.afterResponse(req, message)
        if (!ToolLoop.continues(message, calls) || n >= math.max(options.maxRequests, 1))
          ZIO.succeed(ToolRun(message, n, total, next))
        else if (message.stopReason.contains(StopReason.ToolUse)) withResults(next, calls).flatMap(loop(_, n, total))
        else loop(next, n, total)
      }
    }

    ZIO.suspendSucceed {
      val pending = request.messages.lastOption.map(ToolLoop.unansweredCalls).getOrElse(Vector.empty)
      val start   = if (pending.isEmpty) ZIO.succeed(request) else withResults(request, pending)
      start.flatMap(loop(_, 0, Usage()))
    }
  }

  // ---- Message Batches -------------------------------------------------------------------------

  def createBatch(requests: Seq[BatchRequest], options: CallOptions = CallOptions.default)
      : IO[AnthropicException, MessageBatch] =
    run(underlying.createBatch(requests, options))

  def batch(id: String, options: CallOptions = CallOptions.default): IO[AnthropicException, MessageBatch] =
    run(underlying.batch(id, options))

  /** Polls the batch every `pollEvery` (sleeping on the runtime, not on a thread) until it has ended. */
  def awaitBatch(id: String, pollEvery: FiniteDuration = 30.seconds): IO[AnthropicException, MessageBatch] =
    batch(id).flatMap { b =>
      if (b.isEnded) ZIO.succeed(b) else ZIO.sleep(ZDuration.fromScala(pollEvery)) *> awaitBatch(id, pollEvery)
    }

  def listBatches(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)
      : IO[AnthropicException, Page[MessageBatch]] =
    run(underlying.listBatches(params, options))

  /** Every batch, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allBatches(pageSize: Int = 100): ZStream[Any, AnthropicException, MessageBatch] =
    AnthropicClientZIO.pages(pageSize)(listBatches(_))

  def cancelBatch(id: String, options: CallOptions = CallOptions.default): IO[AnthropicException, MessageBatch] =
    run(underlying.cancelBatch(id, options))

  def deleteBatch(id: String, options: CallOptions = CallOptions.default): IO[AnthropicException, Deleted] =
    run(underlying.deleteBatch(id, options))

  /** The results of an ended batch, decoded line by line as they download. */
  def batchResults(id: String, options: CallOptions = CallOptions.default)
      : ZStream[Any, AnthropicException, BatchResult] =
    iterate(run(underlying.batchResults(id, options)), 16)

  // ---- Files -----------------------------------------------------------------------------------

  def uploadFile(filename: String, mimeType: String, data: Array[Byte], options: CallOptions = CallOptions.default)
      : IO[AnthropicException, FileMetadata] =
    run(underlying.uploadFile(filename, mimeType, data, options))

  def listFiles(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)
      : IO[AnthropicException, Page[FileMetadata]] =
    run(underlying.listFiles(params, options))

  /** Every file, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allFiles(pageSize: Int = 100): ZStream[Any, AnthropicException, FileMetadata] =
    AnthropicClientZIO.pages(pageSize)(listFiles(_))

  def file(id: String, options: CallOptions = CallOptions.default): IO[AnthropicException, FileMetadata] =
    run(underlying.file(id, options))

  def downloadFile(id: String, options: CallOptions = CallOptions.default): IO[AnthropicException, Array[Byte]] =
    run(underlying.downloadFile(id, options))

  def deleteFile(id: String, options: CallOptions = CallOptions.default): IO[AnthropicException, Deleted] =
    run(underlying.deleteFile(id, options))

  // ---- Models ----------------------------------------------------------------------------------

  def listModels(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)
      : IO[AnthropicException, Page[Model]] =
    run(underlying.listModels(params, options))

  /** Every model, newest first, fetching pages of `pageSize` as the stream is pulled. */
  def allModels(pageSize: Int = 100): ZStream[Any, AnthropicException, Model] =
    AnthropicClientZIO.pages(pageSize)(listModels(_))

  def model(id: String, options: CallOptions = CallOptions.default): IO[AnthropicException, Model] =
    run(underlying.model(id, options))

  override def toString: String = s"AnthropicClientZIO(${underlying.config})"
}

object AnthropicClientZIO {

  /** A client released with the scope. */
  def scoped(config: ClientConfig): ZIO[Scope, AnthropicException, AnthropicClientZIO] =
    ZIO.fromAutoCloseable(run(AnthropicClient(config))).map(new AnthropicClientZIO(_))

  /** A client configured from `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` / `ANTHROPIC_BASE_URL`,
    * released with the scope; fails with a `ConfigError` without a credential.
    */
  val fromEnv: ZIO[Scope, AnthropicException, AnthropicClientZIO] =
    ZIO.fromAutoCloseable(run(AnthropicClient.fromEnv())).map(new AnthropicClientZIO(_))

  /** A layer with a client for `config`. */
  def layer(config: ClientConfig): ZLayer[Any, AnthropicException, AnthropicClientZIO] = ZLayer.scoped(scoped(config))

  /** A layer with a client configured from the environment (see [[fromEnv]]). */
  val live: ZLayer[Any, AnthropicException, AnthropicClientZIO] = ZLayer.scoped(fromEnv)

  /** Wraps a client whose lifetime is managed elsewhere. */
  def wrap(client: AnthropicClient): AnthropicClientZIO = new AnthropicClientZIO(client)

  /** Every item of a list endpoint, page after page. */
  def pages[R, E, A](pageSize: Int)(fetch: ListParams => ZIO[R, E, Page[A]]): ZStream[R, E, A] =
    ZStream.paginateChunkZIO(ListParams.limit(pageSize))(p =>
      fetch(p).map(page => (Chunk.fromIterable(page.data), page.nextParams(p)))
    )

  /** The text of a streamed message, piece by piece: `events.via(textDeltas)`. */
  val textDeltas: ZPipeline[Any, Nothing, StreamEvent, String] =
    ZPipeline.collect[StreamEvent, String] { case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(t)) => t }

  /** The message as accumulated after each event (from `message_start` on): the last one is the final
    * message. `events.via(accumulate).runLast` gives it.
    */
  val accumulate: ZPipeline[Any, Nothing, StreamEvent, Message] =
    ZPipeline.suspend {
      val acc = new Accumulator
      ZPipeline.map[StreamEvent, Option[Message]] { e => acc(e); acc.message } >>>
        ZPipeline.collect[Option[Message], Message] { case Some(m) => m }
    }

  /** `thunk` on the blocking pool, its thread interrupted when the fiber is. Anything thrown that is
    * not an `anthropic.AnthropicException` is a defect.
    */
  private def run[A](thunk: => A): IO[AnthropicException, A] =
    ZIO.attemptBlockingInterrupt(thunk).refineToOrDie[AnthropicException]

  /** The items of a blocking iterator, read `chunk` at a time, closing it at the end. */
  private def iterate[A](open: IO[AnthropicException, Iterator[A] & AutoCloseable], chunk: Int)
      : ZStream[Any, AnthropicException, A] =
    ZStream
      .acquireReleaseWith(open)(it => ZIO.succeed(it.close()))
      .flatMap { it =>
        ZStream.repeatZIOChunkOption(
          run {
            val items = Chunk.newBuilder[A]
            var n     = 0
            while (n < chunk && it.hasNext) { items += it.next(); n += 1 }
            items.result()
          }.mapError(Some(_)).flatMap(c => if (c.isEmpty) ZIO.fail(None) else ZIO.succeed(c))
        )
      }
}
