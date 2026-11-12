package anthropic

import scala.collection.immutable.VectorMap

/** The `error` object of an API error response or of an `error` stream event. `kind` is its `type`:
  * `invalid_request_error`, `authentication_error`, `rate_limit_error`, `overloaded_error`...
  */
final case class ApiErrorBody(kind: String, message: String) {
  def toJson: Json = Json.obj("type" -> Json.Str(kind), "message" -> Json.Str(message))
}

object ApiErrorBody {
  def fromJson(j: Json): ApiErrorBody = {
    val c = Decode.obj(j, "error")
    ApiErrorBody(c.str("type"), c.str("message"))
  }
}

/** Everything the SDK fails with. Sealed, so a match on it is checked for exhaustiveness. Errors never
  * carry the request body or the credential.
  *
  * {{{
  * try client.create(request)
  * catch {
  *   case e: AnthropicException.ApiError if e.status.contains(429) => ... // rate limited
  *   case e: AnthropicException if e.isRetryable                    => ... // try again later
  * }
  * }}}
  */
sealed abstract class AnthropicException(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) {

  /** HTTP status of an API error. */
  def status: Option[Int] = None

  /** Whether the same request may succeed if sent again: connection errors, 408, 409, 429, 5xx (529
    * is "overloaded") and overload/server errors reported inside a stream.
    */
  def isRetryable: Boolean = false
}

object AnthropicException {

  /** The API answered with an error. `status` is `None` for an error sent inside a stream. */
  final case class ApiError(
      override val status: Option[Int],
      kind: String,
      message: String,
      requestId: Option[String]
  ) extends AnthropicException(
        s"Anthropic API error $kind (status ${status.fold("none")(_.toString)}): $message" +
          requestId.fold("")(id => s" [request-id $id]")
      ) {
    override def isRetryable: Boolean = status match {
      case Some(s) => retryableStatus(s)
      case None    => kind == "overloaded_error" || kind == "api_error" || kind == "rate_limit_error"
    }
  }

  /** The connection failed or broke off. */
  final case class TransportError(message: String, underlying: Option[Throwable] = None)
      extends AnthropicException(s"transport error: $message", underlying.orNull) {
    override def isRetryable: Boolean = true
  }

  /** A response could not be decoded. */
  final case class DecodeError(message: String) extends AnthropicException(s"could not decode the response: $message")

  /** The call's deadline passed (before an attempt, during one, or between retries). */
  final case class DeadlineExceeded() extends AnthropicException("deadline exceeded")

  final case class ConfigError(message: String) extends AnthropicException(s"invalid configuration: $message")

  /** The request breaks a rule of the configured API version; it was not sent. */
  final case class InvalidRequest(message: String) extends AnthropicException(s"invalid request: $message")

  private[anthropic] def retryableStatus(status: Int): Boolean =
    status == 408 || status == 409 || status == 429 || status >= 500
}

/** Thrown by the decoders below; turned into [[AnthropicException.DecodeError]] at the API boundary. */
final private[anthropic] class DecodeFailure(msg: String) extends Exception(msg)

/** Reading the API's JSON: helpers that fail with [[DecodeFailure]] on a missing or mistyped field. */
private[anthropic] object Decode {

  def fail(msg: String): Nothing = throw new DecodeFailure(msg)

  def obj(j: Json, what: String): Fields = j match {
    case Json.Obj(f) => new Fields(f, what)
    case other       => fail(s"$what: expected an object, got ${other.render.take(80)}")
  }

  def arr(j: Json, what: String): Vector[Json] = j match {
    case Json.Arr(xs) => xs
    case Json.Null    => Vector.empty
    case other        => fail(s"$what: expected an array, got ${other.render.take(80)}")
  }

  /** The `type` of an object, if it has one. */
  def kind(j: Json): Option[String] = j.get("type").flatMap(_.asString)

  /** Runs a decoder, turning its failure into the SDK's error. */
  def run[A](what: String)(f: => A): A =
    try f
    catch {
      case e: DecodeFailure => throw AnthropicException.DecodeError(s"$what: ${e.getMessage}")
    }

  def parse(text: String, what: String): Json = Json.parse(text) match {
    case Right(j) => j
    case Left(e)  => throw AnthropicException.DecodeError(s"$what: $e")
  }

  final class Fields(val fields: VectorMap[String, Json], what: String) {
    private def present(k: String): Option[Json] = fields.get(k).filterNot(_.isNull)

    def has(k: String): Boolean = present(k).isDefined

    def json(k: String): Json = fields.getOrElse(k, Json.Null)

    def req(k: String): Json = present(k).getOrElse(fail(s"$what: missing field `$k`"))

    def str(k: String): String = req(k).asString.getOrElse(fail(s"$what: `$k` is not a string"))
    def strOr(k: String, default: String): String = optStr(k).getOrElse(default)
    def optStr(k: String): Option[String] =
      present(k).map(v => v.asString.getOrElse(fail(s"$what: `$k` is not a string")))

    def long(k: String): Long = req(k).asLong.getOrElse(fail(s"$what: `$k` is not an integer"))
    def longOr(k: String, default: Long): Long = optLong(k).getOrElse(default)
    def optLong(k: String): Option[Long] =
      present(k).map(v => v.asLong.getOrElse(fail(s"$what: `$k` is not an integer")))

    def int(k: String): Int = req(k).asInt.getOrElse(fail(s"$what: `$k` is not an integer"))

    def boolOr(k: String, default: Boolean): Boolean =
      present(k).map(v => v.asBoolean.getOrElse(fail(s"$what: `$k` is not a boolean"))).getOrElse(default)

    def arr(k: String): Vector[Json] = Decode.arr(json(k), s"$what.$k")

    def objOr(k: String): VectorMap[String, Json] = present(k) match {
      case Some(Json.Obj(f)) => f
      case Some(other)       => fail(s"$what: `$k` is not an object, got ${other.render.take(80)}")
      case None              => VectorMap.empty
    }

    /** The fields not in `known`, kept so they round-trip (`type` included unless listed). */
    def extra(known: String*): VectorMap[String, Json] = {
      val k = known.toSet
      fields.filterNot { case (name, _) => k(name) }
    }
  }

  /** Adds `key -> value` when the value is defined. */
  def opt[A](key: String, value: Option[A])(f: A => Json): Option[(String, Json)] = value.map(v => key -> f(v))
}
