package anthropic.monixtask

import _root_.monix.eval.Task
import _root_.monix.reactive.Observable
import anthropic._
import cats.effect.Resource
import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._
import scala.util.control.NonFatal

/** The `anthropic.AnthropicClient` for Monix 3: every call is a `Task`, cancelable (cancelling it
  * aborts the HTTP exchange and stops the retry loop), and every body read piece by piece is an
  * `Observable` that releases its connection when it completes, fails or is cancelled.
  *
  * Calls block a thread of a dedicated, unbounded pool of daemon threads (never the `Scheduler`
  * running the `Task`), and continue on the `Scheduler` afterwards.
  *
  * {{{
  * AnthropicClientTask.fromEnv.use { client =>
  *   client
  *     .stream(MessagesRequest("claude-opus-5-5", 1024).user("Salut!"))
  *     .transform(AnthropicClientTask.textDeltas)
  *     .mapEval(t => Task(print(t)))
  *     .completedL
  * }
  * }}}
  */
final class AnthropicClientTask private (val underlying: AnthropicClient) {
  import AnthropicClientTask.{iterate, run}

  // ---- Messages --------------------------------------------------------------------------------

  /** `POST /v1/messages`. */
  def create(request: MessagesRequest, options: CallOptions = CallOptions.default): Task[Message] =
    run(underlying.create(request, options))

  /** The events of a streamed message. Opening the stream is retried; an error after the first event
    * fails the observable (an `error` event as an `AnthropicException.ApiError` with no status).
    * Fold it into the message with [[AnthropicClientTask.accumulate]], or keep the text with
    * [[AnthropicClientTask.textDeltas]].
    */
  def stream(request: MessagesRequest, options: CallOptions = CallOptions.default): Observable[StreamEvent] =
    iterate(run(underlying.stream(request, options)), 1)

  /** Streams the request and returns the accumulated message, for long outputs or large `max_tokens`. */
  def createStreamed(request: MessagesRequest, options: CallOptions = CallOptions.default): Task[Message] =
    run(underlying.createStreamed(request, options))

  /** `POST /v1/messages/count_tokens`. */
  def countTokens(request: MessagesRequest, options: CallOptions = CallOptions.default): Task[Long] =
    run(underlying.countTokens(request, options))

  /** The tool-use loop of `anthropic.ToolLoop` with a `Task` handler: the calls of one turn run
    * concurrently when `options.parallel` is set, their results go back in call order, and a handler
    * that fails sends its error's message back as an error result. The conversation, grown by every
    * turn, is `ToolRun.request`; run the loop on it again to resume a run cut short.
    */
  def runTools(request: MessagesRequest, options: ToolLoopOptions = ToolLoopOptions())(
      handler: ToolCall => Task[ToolOutput]
  ): Task[ToolRun] = {
    def runCalls(calls: Vector[ToolCall]): Task[Vector[ContentBlockParam]] = {
      def one(call: ToolCall): Task[ContentBlockParam] =
        Task
          .defer(handler(call))
          .onErrorHandle(e => ToolOutput.error(ToolOutput.errorText(e)))
          .map(_.toBlock(call.id))
      (if (options.parallel && calls.length > 1) Task.parTraverse(calls)(one) else Task.traverse(calls)(one)).map(_.toVector)
    }
    def withResults(req: MessagesRequest, calls: Vector[ToolCall]): Task[MessagesRequest] =
      runCalls(calls).map(results => req.message(MessageParam.User(Content.Blocks(results))))

    def loop(req: MessagesRequest, requests: Int, usage: Usage): Task[ToolRun] = {
      val send = if (options.stream) createStreamed(req, options.call) else create(req, options.call)
      send.flatMap { message =>
        val n             = requests + 1
        val total         = usage + message.usage
        val (next, calls) = ToolLoop.afterResponse(req, message)
        if (!ToolLoop.continues(message, calls) || n >= math.max(options.maxRequests, 1))
          Task.pure(ToolRun(message, n, total, next))
        else if (message.stopReason.contains(StopReason.ToolUse)) withResults(next, calls).flatMap(loop(_, n, total))
        else loop(next, n, total)
      }
    }

    Task.defer {
      val pending = request.messages.lastOption.map(ToolLoop.unansweredCalls).getOrElse(Vector.empty)
      val start   = if (pending.isEmpty) Task.pure(request) else withResults(request, pending)
      start.flatMap(loop(_, 0, Usage()))
    }
  }

  // ---- Message Batches -------------------------------------------------------------------------

  def createBatch(requests: Seq[BatchRequest], options: CallOptions = CallOptions.default): Task[MessageBatch] =
    run(underlying.createBatch(requests, options))

  def batch(id: String, options: CallOptions = CallOptions.default): Task[MessageBatch] =
    run(underlying.batch(id, options))

  /** Polls the batch every `pollEvery` (sleeping on the scheduler, not on a thread) until it has ended. */
  def awaitBatch(id: String, pollEvery: FiniteDuration = 30.seconds): Task[MessageBatch] =
    batch(id).flatMap(b => if (b.isEnded) Task.pure(b) else Task.sleep(pollEvery).flatMap(_ => awaitBatch(id, pollEvery)))

  def listBatches(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)
      : Task[Page[MessageBatch]] =
    run(underlying.listBatches(params, options))

  /** Every batch, newest first, fetching pages of `pageSize` as the observable is consumed. */
  def allBatches(pageSize: Int = 100): Observable[MessageBatch] = AnthropicClientTask.pages(pageSize)(listBatches(_))

  def cancelBatch(id: String, options: CallOptions = CallOptions.default): Task[MessageBatch] =
    run(underlying.cancelBatch(id, options))

  def deleteBatch(id: String, options: CallOptions = CallOptions.default): Task[Deleted] =
    run(underlying.deleteBatch(id, options))

  /** The results of an ended batch, decoded line by line as they download. */
  def batchResults(id: String, options: CallOptions = CallOptions.default): Observable[BatchResult] =
    iterate(run(underlying.batchResults(id, options)), 16)

  // ---- Files -----------------------------------------------------------------------------------

  def uploadFile(filename: String, mimeType: String, data: Array[Byte], options: CallOptions = CallOptions.default)
      : Task[FileMetadata] =
    run(underlying.uploadFile(filename, mimeType, data, options))

  def listFiles(params: ListParams = ListParams(), options: CallOptions = CallOptions.default)
      : Task[Page[FileMetadata]] =
    run(underlying.listFiles(params, options))

  /** Every file, newest first, fetching pages of `pageSize` as the observable is consumed. */
  def allFiles(pageSize: Int = 100): Observable[FileMetadata] = AnthropicClientTask.pages(pageSize)(listFiles(_))

  def file(id: String, options: CallOptions = CallOptions.default): Task[FileMetadata] =
    run(underlying.file(id, options))

  def downloadFile(id: String, options: CallOptions = CallOptions.default): Task[Array[Byte]] =
    run(underlying.downloadFile(id, options))

  def deleteFile(id: String, options: CallOptions = CallOptions.default): Task[Deleted] =
    run(underlying.deleteFile(id, options))

  // ---- Models ----------------------------------------------------------------------------------

  def listModels(params: ListParams = ListParams(), options: CallOptions = CallOptions.default): Task[Page[Model]] =
    run(underlying.listModels(params, options))

  /** Every model, newest first, fetching pages of `pageSize` as the observable is consumed. */
  def allModels(pageSize: Int = 100): Observable[Model] = AnthropicClientTask.pages(pageSize)(listModels(_))

  def model(id: String, options: CallOptions = CallOptions.default): Task[Model] = run(underlying.model(id, options))

  override def toString: String = s"AnthropicClientTask(${underlying.config})"
}

object AnthropicClientTask {

  /** A client released with the resource. */
  def apply(config: ClientConfig): Resource[Task, AnthropicClientTask] =
    Resource.fromAutoCloseable(Task(AnthropicClient(config))).map(new AnthropicClientTask(_))

  /** A client configured from `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` / `ANTHROPIC_BASE_URL`; the
    * resource fails without a credential.
    */
  def fromEnv: Resource[Task, AnthropicClientTask] =
    Resource.fromAutoCloseable(Task(AnthropicClient.fromEnv())).map(new AnthropicClientTask(_))

  /** Wraps a client whose lifetime is managed elsewhere. */
  def wrap(client: AnthropicClient): AnthropicClientTask = new AnthropicClientTask(client)

  /** Every item of a list endpoint, page after page. */
  def pages[A](pageSize: Int)(fetch: ListParams => Task[Page[A]]): Observable[A] =
    Observable
      .unfoldEval(Option(ListParams.limit(pageSize))) {
        case Some(p) => fetch(p).map(page => Option((page.data, page.nextParams(p))))
        case None    => Task.pure(Option.empty[(Seq[A], Option[ListParams])])
      }
      .concatMapIterable(_.toList)

  /** The text of a streamed message, piece by piece: `events.transform(textDeltas)`. */
  val textDeltas: Observable[StreamEvent] => Observable[String] =
    _.collect { case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(t)) => t }

  /** The message as accumulated after each event (from `message_start` on): the last one is the final
    * message. `events.transform(accumulate).lastL` gives it.
    */
  val accumulate: Observable[StreamEvent] => Observable[Message] =
    events =>
      Observable.defer {
        val acc = new Accumulator
        events.map { e => acc(e); acc.message }.collect { case Some(m) => m }
      }

  // ---- blocking calls --------------------------------------------------------------------------

  /** Threads for the blocking calls: as many as there are calls in flight, reclaimed when idle. */
  private lazy val blockingPool: ExecutorService = {
    val count = new AtomicInteger(0)
    val factory: ThreadFactory = { (r: Runnable) =>
      val t = new Thread(r, s"anthropic-sdk-monix-${count.incrementAndGet()}")
      t.setDaemon(true)
      t
    }
    Executors.newCachedThreadPool(factory)
  }

  /** `thunk` on a blocking thread; cancelling the task interrupts that thread, and does not wait for it. */
  private def run[A](thunk: => A): Task[A] =
    Task
      .cancelable[A] { cb =>
        val done = new AtomicBoolean(false)
        val work: Runnable = { () =>
          val result =
            try Right(thunk)
            catch {
              case e: InterruptedException => Left(e)
              case NonFatal(e)             => Left(e)
            }
          if (done.compareAndSet(false, true)) cb(result)
        }
        val future = blockingPool.submit(work)
        Task { if (done.compareAndSet(false, true)) { future.cancel(true); () } }
      }
      .asyncBoundary

  /** The items of a blocking iterator, read `chunk` at a time, closing it at the end. */
  private def iterate[A](open: Task[Iterator[A] & AutoCloseable], chunk: Int): Observable[A] =
    Observable
      .resource(open)(it => Task(it.close()))
      .flatMap { it =>
        Observable
          .repeatEvalF(run {
            val items = List.newBuilder[A]
            var n     = 0
            while (n < chunk && it.hasNext) { items += it.next(); n += 1 }
            items.result()
          })
          .takeWhile(_.nonEmpty)
          .concatMapIterable(identity)
      }
}
