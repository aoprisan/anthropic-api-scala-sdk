package anthropic

import java.io.IOException
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.http.HttpRequest.BodyPublishers
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.{CancellationException, ExecutionException, TimeUnit, TimeoutException}
import scala.concurrent.duration._
import scala.util.control.NonFatal

/** How requests authenticate. `toString` never shows the secret. */
sealed trait Credential extends Product with Serializable

object Credential {

  /** An API key, sent as `Authorization: Bearer` (the API accepts a key in either header). */
  final case class ApiKey(key: String) extends Credential { override def toString = "ApiKey(..)" }

  /** An API key sent as `x-api-key`, for a proxy or gateway that reads that header. */
  final case class XApiKey(key: String) extends Credential { override def toString = "XApiKey(..)" }

  /** An OAuth access token, sent as `Authorization: Bearer` with the OAuth beta. */
  final case class Bearer(token: String) extends Credential { override def toString = "Bearer(..)" }
}

/** Client settings.
  *
  * @param baseUrl        API root, `https://api.anthropic.com` by default
  * @param timeout        timeout of one non-streaming attempt, and of waiting for a stream's headers
  * @param maxRetries     extra attempts after a retryable failure
  * @param betas          `anthropic-beta` values sent with every request
  * @param apiVersion     the `anthropic-version` header, [[AnthropicClient.ApiVersion]] by default. From
  *                       `2026-10-21` on, Messages requests are checked with [[MessagesRequest.validate]]
  *                       before they are sent
  * @param httpClient     a JDK `HttpClient` to use (proxy, SSL, executor...); the client builds one otherwise
  */
final case class ClientConfig(
    credential: Credential,
    baseUrl: String = AnthropicClient.DefaultBaseUrl,
    timeout: FiniteDuration = 600.seconds,
    connectTimeout: FiniteDuration = 10.seconds,
    maxRetries: Int = 2,
    betas: Vector[String] = Vector.empty,
    apiVersion: String = AnthropicClient.ApiVersion,
    httpClient: Option[HttpClient] = None
)

object ClientConfig {
  def apiKey(key: String): ClientConfig = ClientConfig(Credential.ApiKey(key))

  /** One credential, `ANTHROPIC_API_KEY` or else `ANTHROPIC_AUTH_TOKEN` (a bearer token), plus
    * `ANTHROPIC_BASE_URL` when set, as the official SDKs read them. Blank values are ignored.
    */
  def fromEnv(env: String => Option[String] = sys.env.get): Option[ClientConfig] = {
    def v(k: String) = env(k).map(_.trim).filter(_.nonEmpty)
    v("ANTHROPIC_API_KEY")
      .map(k => Credential.ApiKey(k): Credential)
      .orElse(v("ANTHROPIC_AUTH_TOKEN").map(Credential.Bearer(_)))
      .map { credential =>
        val c = ClientConfig(credential)
        v("ANTHROPIC_BASE_URL").fold(c)(u => c.copy(baseUrl = u))
      }
  }
}

/** Per-call settings.
  *
  * @param deadline   when the whole call (every attempt, every wait, and for a stream every read) must end
  * @param maxRetries overrides [[ClientConfig.maxRetries]]
  */
final case class CallOptions(deadline: Option[Deadline] = None, maxRetries: Option[Int] = None)

object CallOptions {
  val default: CallOptions = CallOptions()

  /** A deadline `budget` from now. */
  def within(budget: FiniteDuration): CallOptions = CallOptions(deadline = Some(budget.fromNow))
}

/** A client for Anthropic's Messages API (`POST /v1/messages`) and the endpoints around it. Calls block
  * the calling thread (cheap on virtual threads) and throw [[AnthropicException]]; the
  * `anthropic-api-scala-sdk-cats-effect`, `-zio` and `-monix` modules wrap them for Cats Effect and fs2,
  * ZIO and Monix. Thread-safe: share one instance. An `AutoCloseable`, so `scala.util.Using(AnthropicClient(...))` releases it.
  *
  * {{{
  * val client  = AnthropicClient(ClientConfig.fromEnv().getOrElse(sys.error("ANTHROPIC_API_KEY")))
  * val message = client.create(MessagesRequest("claude-opus-5-5", 1024).user("Salut!"))
  * if (!message.isRefusal) println(message.text)
  * }}}
  */
final class AnthropicClient(val config: ClientConfig) extends AutoCloseable {
  import AnthropicClient._

  private val ownsHttp = config.httpClient.isEmpty
  private val http: HttpClient = config.httpClient.getOrElse(
    HttpClient
      .newBuilder()
      .connectTimeout(java.time.Duration.ofMillis(config.connectTimeout.toMillis))
      .followRedirects(HttpClient.Redirect.NORMAL)
      .build()
  )

  // Fail at construction on a base URL that could never work.
  locally(Http.url(config.baseUrl, Nil, Nil))

  override def toString: String = s"AnthropicClient($config)"

  // ---- Messages --------------------------------------------------------------------------------

  /** `POST /v1/messages`. */
  def create(request: MessagesRequest, options: CallOptions = CallOptions.default): Message = {
    check(request)
    val body = request.toJson.render.getBytes(UTF_8)
    callJson("POST", Seq("v1", "messages"), Nil, Payload.JsonBody(body), request.betaHeaderValues, options)(
      "message",
      Message.fromJson
    )
  }

  /** `POST /v1/messages` with `"stream": true`. Retries cover opening the stream only; an error after the
    * first event is thrown while reading the stream. Close the stream, or read it to its end.
    */
  def stream(request: MessagesRequest, options: CallOptions = CallOptions.default): MessageStream = {
    check(request)
    val body = (request.toJson + ("stream" -> Json.True)).render.getBytes(UTF_8)
    val uri  = Http.url(config.baseUrl, Seq("v1", "messages"), Nil)
    retrying(options) {
      send("POST", uri, Payload.JsonBody(body), request.betaHeaderValues, options).map { case (res, body) =>
        new MessageStream(body.watch(options.deadline), header(res, "request-id"))
      }
    }
  }

  /** Streams the request and returns the accumulated message, for long outputs or large `max_tokens`. */
  def createStreamed(request: MessagesRequest, options: CallOptions = CallOptions.default): Message =
    stream(request, options).finalMessage()

  /** `POST /v1/messages/count_tokens`: input tokens of the request's prompt on its model. */
  def countTokens(request: MessagesRequest, options: CallOptions = CallOptions.default): Long = {
    check(request)
    val body = request.countTokensJson.render.getBytes(UTF_8)
    callJson("POST", Seq("v1", "messages", "count_tokens"), Nil, Payload.JsonBody(body), request.betas, options)(
      "count_tokens",
      j => Decode.obj(j, "count_tokens").long("input_tokens")
    )
  }

  /** Runs the tool-use loop on `request`: see [[ToolLoop]]. The conversation, grown by every turn, is
    * [[ToolRun.request]]; pass it back to resume a run cut short or failed.
    *
    * {{{
    * val run = client.runTools(request) { call =>
    *   call.name match {
    *     case "get_weather" => ToolOutput.text("Sunny in " + call.input("city").asString.getOrElse("?"))
    *     case other         => ToolOutput.error("unknown tool " + other)
    *   }
    * }
    * if (run.isFinished && !run.message.isRefusal) println(run.message.text)
    * }}}
    */
  def runTools(request: MessagesRequest, options: ToolLoopOptions = ToolLoopOptions())(
      handler: ToolCall => ToolOutput
  ): ToolRun =
    ToolLoop.run(this, request, options, handler)

  // ---- Message Batches -------------------------------------------------------------------------

  /** `POST /v1/messages/batches`. The beta headers the requests need are sent with it. */
  def createBatch(requests: Seq[BatchRequest], options: CallOptions = CallOptions.default): MessageBatch = {
    requests.foreach { r =>
      checked(r.params).left.foreach(e => throw AnthropicException.InvalidRequest(s"${r.customId}: ${e.message}"))
    }
    val betas = requests.flatMap(_.params.betaHeaderValues).distinct
    val body  = Json.obj("requests" -> Json.Arr(requests.map(_.toJson).toVector)).render.getBytes(UTF_8)
    callJson("POST", Seq("v1", "messages", "batches"), Nil, Payload.JsonBody(body), betas, options)(
      "message batch",
      MessageBatch.fromJson
    )
  }

  /** `GET /v1/messages/batches/{id}`. */
  def batch(id: String, options: CallOptions = CallOptions.default): MessageBatch =
    callJson("GET", Seq("v1", "messages", "batches", id), Nil, Payload.Empty, Nil, options)("message batch", MessageBatch.fromJson)

  /** Polls the batch every `pollEvery` until it has ended (or the deadline of `options` passes), and
    * returns it ended. Batches usually take minutes, and at most 24 hours.
    */
  def awaitBatch(id: String, pollEvery: FiniteDuration = 30.seconds, options: CallOptions = CallOptions.default)
      : MessageBatch = {
    var current = batch(id, options)
    while (!current.isEnded) {
      if (options.deadline.exists(d => (pollEvery.fromNow - d) >= Duration.Zero)) throw AnthropicException.DeadlineExceeded()
      Thread.sleep(pollEvery.toMillis)
      current = batch(id, options)
    }
    current
  }

  /** `GET /v1/messages/batches`: one page, newest first. */
  def listBatches(params: ListParams = ListParams(), options: CallOptions = CallOptions.default): Page[MessageBatch] =
    callJson("GET", Seq("v1", "messages", "batches"), params.query, Payload.Empty, Nil, options)(
      "message batches",
      Page.fromJson(_)(MessageBatch.fromJson)
    )

  /** `POST /v1/messages/batches/{id}/cancel`: the batch moves to `Canceling`, then `Ended`; requests
    * already processed keep their results.
    */
  def cancelBatch(id: String, options: CallOptions = CallOptions.default): MessageBatch =
    callJson("POST", Seq("v1", "messages", "batches", id, "cancel"), Nil, Payload.Empty, Nil, options)(
      "message batch",
      MessageBatch.fromJson
    )

  /** `DELETE /v1/messages/batches/{id}`: only an ended batch can be deleted. */
  def deleteBatch(id: String, options: CallOptions = CallOptions.default): Deleted =
    callJson("DELETE", Seq("v1", "messages", "batches", id), Nil, Payload.Empty, Nil, options)("deleted", Deleted.fromJson)

  /** `GET /v1/messages/batches/{id}/results`: the results of an ended batch, read line by line as they
    * download (in any order; match them by `customId`). Close it, or read it to its end.
    */
  def batchResults(id: String, options: CallOptions = CallOptions.default): BatchResults = {
    val uri = Http.url(config.baseUrl, Seq("v1", "messages", "batches", id, "results"), Nil)
    retrying(options) {
      send("GET", uri, Payload.Empty, Nil, options).map { case (_, body) => new BatchResults(body.watch(options.deadline)) }
    }
  }

  // ---- Files -----------------------------------------------------------------------------------

  /** `POST /v1/files` (multipart). The block that uses the file must match its type: `document` for PDF
    * and plain text, `image` for images.
    */
  def uploadFile(filename: String, mimeType: String, data: Array[Byte], options: CallOptions = CallOptions.default)
      : FileMetadata =
    callJson("POST", Seq("v1", "files"), Nil, Payload.File(filename, mimeType, data), Nil, options)(
      "file",
      FileMetadata.fromJson
    )

  /** Uploads a file from disk under its own name. */
  def uploadFile(path: Path, mimeType: String): FileMetadata =
    uploadFile(path.getFileName.toString, mimeType, Files.readAllBytes(path))

  /** `GET /v1/files`: one page, newest first. */
  def listFiles(params: ListParams = ListParams(), options: CallOptions = CallOptions.default): Page[FileMetadata] =
    callJson("GET", Seq("v1", "files"), params.query, Payload.Empty, Nil, options)("files", Page.fromJson(_)(FileMetadata.fromJson))

  /** `GET /v1/files/{id}`. */
  def file(id: String, options: CallOptions = CallOptions.default): FileMetadata =
    callJson("GET", Seq("v1", "files", id), Nil, Payload.Empty, Nil, options)("file", FileMetadata.fromJson)

  /** `GET /v1/files/{id}/content`: the file's bytes (downloadable files only). */
  def downloadFile(id: String, options: CallOptions = CallOptions.default): Array[Byte] =
    callBytes("GET", Seq("v1", "files", id, "content"), Nil, Payload.Empty, Nil, options)

  /** `DELETE /v1/files/{id}`. */
  def deleteFile(id: String, options: CallOptions = CallOptions.default): Deleted =
    callJson("DELETE", Seq("v1", "files", id), Nil, Payload.Empty, Nil, options)("deleted", Deleted.fromJson)

  // ---- Models ----------------------------------------------------------------------------------

  /** `GET /v1/models`: one page, newest first. */
  def listModels(params: ListParams = ListParams(), options: CallOptions = CallOptions.default): Page[Model] =
    callJson("GET", Seq("v1", "models"), params.query, Payload.Empty, Nil, options)("models", Page.fromJson(_)(Model.fromJson))

  /** `GET /v1/models/{id}`; `id` may be an alias. */
  def model(id: String, options: CallOptions = CallOptions.default): Model =
    callJson("GET", Seq("v1", "models", id), Nil, Payload.Empty, Nil, options)("model", Model.fromJson)

  // ---- Transport -------------------------------------------------------------------------------

  /** Releases the HTTP client this client built (on Java 21 and later); a client passed in through
    * [[ClientConfig.httpClient]] is left open.
    */
  override def close(): Unit =
    if (ownsHttp) (http: AnyRef) match {
      case c: AutoCloseable => c.close()
      case _                => ()
    }

  /** The request, or why the configured API version would reject it. */
  private[anthropic] def checked(request: MessagesRequest): Either[AnthropicException.InvalidRequest, Unit] =
    if (config.apiVersion >= ApiVersion) request.validate else Right(())

  /** Rejects, before sending, a request the configured API version would reject. */
  private[anthropic] def check(request: MessagesRequest): Unit = checked(request).left.foreach(e => throw e)

  private def callJson[A](
      method: String,
      path: Seq[String],
      query: Seq[(String, String)],
      payload: Payload,
      betas: Seq[String],
      options: CallOptions
  )(what: String, decode: Json => A): A = {
    val bytes = callBytes(method, path, query, payload, betas, options)
    Decode.run(what)(decode(Decode.parse(new String(bytes, UTF_8), what)))
  }

  /** One request with retries; the whole body. */
  private def callBytes(
      method: String,
      path: Seq[String],
      query: Seq[(String, String)],
      payload: Payload,
      betas: Seq[String],
      options: CallOptions
  ): Array[Byte] = {
    val uri = Http.url(config.baseUrl, path, query)
    retrying(options) {
      send(method, uri, payload, betas, options).flatMap { case (_, body) =>
        try Right(body.watch(Some(attemptEnd(options))).readAll())
        catch { case e: AnthropicException => Left(Failure(e, retry = true, None)) }
      }
    }
  }

  private def retrying[A](options: CallOptions)(attempt: => Either[Failure, A]): A = {
    val max   = options.maxRetries.getOrElse(config.maxRetries)
    var tries = 0
    while (true) {
      if (options.deadline.exists(_.isOverdue())) throw AnthropicException.DeadlineExceeded()
      attempt match {
        case Right(v) => return v
        case Left(failure) =>
          if (!failure.retry || tries >= max) throw failure.error
          val wait = failure.retryAfter.getOrElse(backoff(tries))
          // No time left for the wait and another attempt: report what went wrong.
          if (options.deadline.exists(d => (wait.fromNow - d) >= Duration.Zero)) throw failure.error
          tries += 1
          if (wait > Duration.Zero) Thread.sleep(wait.toMillis)
      }
    }
    throw new IllegalStateException("unreachable")
  }

  private def attemptEnd(options: CallOptions): Deadline = {
    val end = config.timeout.fromNow
    options.deadline.fold(end)(d => if (d < end) d else end)
  }

  private[anthropic] def userAgent: String = s"${BuildInfo.name}/${BuildInfo.version}"

  /** One attempt: the open response on a 2xx, or why it failed. */
  private def send(
      method: String,
      uri: URI,
      payload: Payload,
      betas: Seq[String],
      options: CallOptions
  ): Either[Failure, (HttpResponse[java.io.InputStream], Http.Body)] = {
    val end = attemptEnd(options)
    val b = HttpRequest
      .newBuilder(uri)
      .header("anthropic-version", config.apiVersion)
      .header("User-Agent", userAgent)
    config.credential match {
      case Credential.ApiKey(k)  => b.header("Authorization", s"Bearer $k")
      case Credential.Bearer(t)  => b.header("Authorization", s"Bearer $t")
      case Credential.XApiKey(k) => b.header("x-api-key", k)
    }
    val oauth = config.credential match {
      case _: Credential.Bearer => Seq(BetaOAuth)
      case _                    => Nil
    }
    val allBetas = (config.betas ++ betas ++ oauth).distinct
    if (allBetas.nonEmpty) b.header("anthropic-beta", allBetas.mkString(","))
    payload match {
      case Payload.Empty =>
        if (method == "POST") b.POST(BodyPublishers.noBody()) else b.method(method, BodyPublishers.noBody())
      case Payload.JsonBody(bytes) =>
        b.header("Content-Type", "application/json").method(method, BodyPublishers.ofByteArray(bytes))
      case Payload.File(filename, mimeType, data) =>
        val (contentType, bytes) = Http.multipart(filename, mimeType, data)
        b.header("Content-Type", contentType).method(method, BodyPublishers.ofByteArray(bytes))
    }
    val future = http.sendAsync(b.build(), HttpResponse.BodyHandlers.ofInputStream())
    val response =
      try Right(future.get(math.max(end.timeLeft.toMillis, 1L), TimeUnit.MILLISECONDS))
      catch {
        case _: TimeoutException =>
          future.cancel(true)
          Left(Failure(AnthropicException.DeadlineExceeded(), retry = true, None))
        case e: InterruptedException =>
          future.cancel(true)
          throw e
        case e: ExecutionException => Left(transport(e.getCause))
        case e: CancellationException => Left(transport(e))
      }
    response.flatMap { res =>
      val body = new Http.Body(res.body())
      val status = res.statusCode
      if (status >= 200 && status < 300) Right((res, body))
      else {
        val text =
          try new String(body.watch(Some(end)).readAll(limit = 1 << 20), UTF_8)
          catch { case NonFatal(_) => "" }
        val (kind, message, bodyId) = Json.parse(text).toOption.flatMap(errorEnvelope) match {
          case Some(e) => e
          case None    => ("http_error", s"HTTP $status", None)
        }
        val retry = header(res, "x-should-retry") match {
          case Some("true")  => true
          case Some("false") => false
          case _             => AnthropicException.retryableStatus(status)
        }
        val wait = header(res, "retry-after")
          .flatMap(v => scala.util.Try(v.trim.toDouble).toOption)
          .filter(s => !s.isNaN && !s.isInfinite && s >= 0)
          .map(s => (s * 1000).toLong.millis)
        Left(Failure(AnthropicException.ApiError(Some(status), kind, message, header(res, "request-id").orElse(bodyId)), retry, wait))
      }
    }
  }
}

object AnthropicClient {
  val DefaultBaseUrl = "https://api.anthropic.com"

  /** Default value of the `anthropic-version` header (see [[ClientConfig.apiVersion]]). */
  val ApiVersion = "2026-10-21"

  /** The previous API version, for requests that still need its behaviour (e.g. sampling parameters on
    * models that accept non-default values). Requests on it are not checked.
    */
  val LegacyApiVersion = "2023-06-01"

  /** Beta value an OAuth access token needs. */
  private val BetaOAuth = "oauth-2025-04-20"

  def apply(config: ClientConfig): AnthropicClient = new AnthropicClient(config)

  /** A client configured from the environment (see [[ClientConfig.fromEnv]]); fails without a credential. */
  def fromEnv(): AnthropicClient = new AnthropicClient(
    ClientConfig.fromEnv().getOrElse(throw AnthropicException.ConfigError("set ANTHROPIC_API_KEY or ANTHROPIC_AUTH_TOKEN"))
  )

  /** What a request carries. */
  sealed private trait Payload
  private object Payload {
    case object Empty                                                  extends Payload
    final case class JsonBody(bytes: Array[Byte])                      extends Payload
    final case class File(filename: String, mimeType: String, data: Array[Byte]) extends Payload
  }

  /** A failed attempt, and whether it may be retried after `wait`. */
  final private case class Failure(error: AnthropicException, retry: Boolean, retryAfter: Option[FiniteDuration])

  private def transport(e: Throwable): Failure = e match {
    case _: HttpTimeoutException => Failure(AnthropicException.DeadlineExceeded(), retry = true, None)
    case e: IOException =>
      Failure(AnthropicException.TransportError(Option(e.getMessage).getOrElse(e.getClass.getSimpleName), Some(e)), retry = true, None)
    case e =>
      Failure(AnthropicException.TransportError(Option(e.getMessage).getOrElse(e.getClass.getSimpleName), Some(e)), retry = true, None)
  }

  private def header(res: HttpResponse[?], name: String): Option[String] = {
    val v = res.headers().firstValue(name)
    if (v.isPresent) Some(v.get) else None
  }

  private def errorEnvelope(j: Json): Option[(String, String, Option[String])] =
    for {
      e    <- j.get("error")
      kind <- e.get("type").flatMap(_.asString)
      msg  <- e.get("message").flatMap(_.asString)
    } yield (kind, msg, j.get("request_id").flatMap(_.asString))

  /** 0.5 s, 1 s, 2 s... capped at 8 s. */
  private def backoff(tries: Int): FiniteDuration = (500L * (1L << math.min(tries, 4))).millis
}
