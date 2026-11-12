package anthropic

import scala.collection.immutable.VectorMap
import scala.collection.mutable

/** One server-sent event of a streamed message. Any event type this SDK does not know is kept verbatim
  * as [[StreamEvent.Other]] (new types may be added at any time).
  */
sealed trait StreamEvent extends Product with Serializable {

  /** The event's `type`. */
  def kind: String

  def toJson: Json = {
    import StreamEvent._
    def typed(fields: (String, Json)*): Json = Json.Obj(VectorMap.from(("type" -> Json.Str(kind)) +: fields))
    this match {
      case MessageStart(m)             => typed("message" -> m.toJson)
      case ContentBlockStart(i, block) => typed("index" -> Json.num(i.toLong), "content_block" -> block.toJson)
      case ContentBlockDelta(i, delta) => typed("index" -> Json.num(i.toLong), "delta" -> delta.toJson)
      case ContentBlockStop(i)         => typed("index" -> Json.num(i.toLong))
      case MessageDelta(delta, usage)  => typed("delta" -> Json.Obj(delta), "usage" -> Json.Obj(usage))
      case MessageStop                 => typed()
      case Ping                        => typed()
      case Error(error)                => typed("error" -> error.toJson)
      case Other(json)                 => json
    }
  }
}

object StreamEvent {
  final case class MessageStart(message: Message) extends StreamEvent { def kind = "message_start" }

  final case class ContentBlockStart(index: Int, contentBlock: ContentBlock) extends StreamEvent {
    def kind = "content_block_start"
  }

  final case class ContentBlockDelta(index: Int, delta: Delta) extends StreamEvent { def kind = "content_block_delta" }

  final case class ContentBlockStop(index: Int) extends StreamEvent { def kind = "content_block_stop" }

  /** `delta` holds `stop_reason`, `stop_sequence`, `stop_details`; `usage` the cumulative counts. */
  final case class MessageDelta(delta: VectorMap[String, Json], usage: VectorMap[String, Json]) extends StreamEvent {
    def kind = "message_delta"
  }

  case object MessageStop extends StreamEvent { def kind = "message_stop" }

  case object Ping extends StreamEvent { def kind = "ping" }

  /** Reading a stream turns this event into an [[AnthropicException.ApiError]] with no status. */
  final case class Error(error: ApiErrorBody) extends StreamEvent { def kind = "error" }

  /** Any other event type, kept verbatim. */
  final case class Other(json: Json) extends StreamEvent {
    def kind: String = Decode.kind(json).getOrElse("")
  }

  val KnownTypes: Set[String] = Set(
    "message_start",
    "content_block_start",
    "content_block_delta",
    "content_block_stop",
    "message_delta",
    "message_stop",
    "ping",
    "error"
  )

  def fromJson(j: Json): StreamEvent = Decode.kind(j) match {
    case Some(k) if KnownTypes(k) =>
      val c = Decode.obj(j, s"$k event")
      k match {
        case "message_start"       => MessageStart(Message.fromJson(c.req("message")))
        case "content_block_start" => ContentBlockStart(c.int("index"), ContentBlock.fromJson(c.req("content_block")))
        case "content_block_delta" => ContentBlockDelta(c.int("index"), Delta.fromJson(c.req("delta")))
        case "content_block_stop"  => ContentBlockStop(c.int("index"))
        case "message_delta"       => MessageDelta(c.objOr("delta"), c.objOr("usage"))
        case "message_stop"        => MessageStop
        case "ping"                => Ping
        case _                     => Error(ApiErrorBody.fromJson(c.req("error")))
      }
    case _ => Other(j)
  }
}

/** A change to the content block being streamed. Unknown delta types are kept as [[Delta.Other]]. */
sealed trait Delta extends Product with Serializable {
  def kind: String

  def toJson: Json = this match {
    case Delta.TextDelta(t)       => Json.obj("type" -> Json.Str(kind), "text" -> Json.Str(t))
    case Delta.InputJsonDelta(p)  => Json.obj("type" -> Json.Str(kind), "partial_json" -> Json.Str(p))
    case Delta.ThinkingDelta(t)   => Json.obj("type" -> Json.Str(kind), "thinking" -> Json.Str(t))
    case Delta.SignatureDelta(s)  => Json.obj("type" -> Json.Str(kind), "signature" -> Json.Str(s))
    case Delta.CitationsDelta(c)  => Json.obj("type" -> Json.Str(kind), "citation" -> c.toJson)
    case Delta.Other(json)        => json
  }
}

object Delta {
  final case class TextDelta(text: String) extends Delta { def kind = "text_delta" }

  /** A piece of a tool call's JSON input; only the concatenation parses. */
  final case class InputJsonDelta(partialJson: String) extends Delta { def kind = "input_json_delta" }

  final case class ThinkingDelta(thinking: String) extends Delta { def kind = "thinking_delta" }

  final case class SignatureDelta(signature: String) extends Delta { def kind = "signature_delta" }

  /** One citation to add to the current text block. */
  final case class CitationsDelta(citation: Citation) extends Delta { def kind = "citations_delta" }

  final case class Other(json: Json) extends Delta { def kind: String = Decode.kind(json).getOrElse("") }

  val KnownTypes: Set[String] = Set("text_delta", "input_json_delta", "thinking_delta", "signature_delta", "citations_delta")

  def fromJson(j: Json): Delta = Decode.kind(j) match {
    case Some(k) if KnownTypes(k) =>
      val c = Decode.obj(j, k)
      k match {
        case "text_delta"       => TextDelta(c.str("text"))
        case "input_json_delta" => InputJsonDelta(c.str("partial_json"))
        case "thinking_delta"   => ThinkingDelta(c.str("thinking"))
        case "signature_delta"  => SignatureDelta(c.str("signature"))
        case _                  => CitationsDelta(Citation.fromJson(c.req("citation")))
      }
    case _ => Other(j)
  }
}

/** Splits a byte stream into the `data` of each server-sent event. */
final private[anthropic] class SseParser {
  private val buf = new java.io.ByteArrayOutputStream

  /** Feeds bytes and returns the data of every event completed by them. */
  def push(bytes: Array[Byte], len: Int): Vector[String] = {
    buf.write(bytes, 0, len)
    val all  = buf.toByteArray
    val out  = Vector.newBuilder[String]
    var from = 0
    var done = false
    while (!done) {
      SseParser.findEventEnd(all, from) match {
        case Some((end, sep)) =>
          SseParser.eventData(new String(all, from, end - from, "UTF-8")).foreach(out += _)
          from = end + sep
        case None => done = true
      }
    }
    buf.reset()
    buf.write(all, from, all.length - from)
    out.result()
  }

  /** Whatever is left when the body ends (an event without its blank line). */
  def finish(): Option[String] = {
    val rest = buf.toString("UTF-8")
    buf.reset()
    SseParser.eventData(rest)
  }
}

private[anthropic] object SseParser {

  /** Position and length of the first blank-line separator (`\n\n`, `\r\n\r\n` or `\r\r`) from `from`. */
  def findEventEnd(buf: Array[Byte], from: Int): Option[(Int, Int)] = {
    var i = from
    while (i < buf.length) {
      if (i + 3 < buf.length && buf(i) == '\r' && buf(i + 1) == '\n' && buf(i + 2) == '\r' && buf(i + 3) == '\n')
        return Some((i, 4))
      if (i + 1 < buf.length && ((buf(i) == '\n' && buf(i + 1) == '\n') || (buf(i) == '\r' && buf(i + 1) == '\r')))
        return Some((i, 2))
      i += 1
    }
    None
  }

  /** The `data` field of one event: its `data:` lines joined with newlines; `None` for an event without
    * data (comments, a bare `event:` line).
    */
  def eventData(event: String): Option[String] = {
    val lines = event.split("[\r\n]", -1).iterator.flatMap { line =>
      if (!line.startsWith("data")) None
      else {
        val rest = line.substring(4)
        if (rest.isEmpty) Some("")
        else if (rest.startsWith(":")) Some(rest.substring(1).stripPrefix(" "))
        else None
      }
    }.toVector
    if (lines.isEmpty) None else Some(lines.mkString("\n"))
  }
}

/** Builds the final message from the events. */
final private[anthropic] class Accumulator {
  private var current: Option[Message]  = None
  private val partialJson                = mutable.Map.empty[Int, java.lang.StringBuilder]

  def message: Option[Message] = current

  private def updateBlock(index: Int)(f: ContentBlock => ContentBlock): Unit =
    current = current.map { m =>
      if (index < 0 || index >= m.content.length) m
      else m.copy(content = m.content.updated(index, f(m.content(index))))
    }

  def apply(event: StreamEvent): Unit = event match {
    case StreamEvent.MessageStart(m) => current = Some(m)
    case StreamEvent.ContentBlockStart(index, block) =>
      current = current.map { m =>
        val padded = if (m.content.length < index) m.content ++ Vector.fill(index - m.content.length)(ContentBlock.Other(Json.Null)) else m.content
        m.copy(content = if (index < padded.length) padded.updated(index, block) else padded :+ block)
      }
    case StreamEvent.ContentBlockDelta(index, Delta.InputJsonDelta(p)) =>
      partialJson.getOrElseUpdate(index, new java.lang.StringBuilder).append(p)
      ()
    case StreamEvent.ContentBlockDelta(index, delta) =>
      updateBlock(index) { block =>
        (block, delta) match {
          case (b: ContentBlock.Text, Delta.TextDelta(t))           => b.copy(text = b.text + t)
          case (b: ContentBlock.Text, Delta.CitationsDelta(c))      => b.copy(citations = b.citations :+ c)
          case (b: ContentBlock.Thinking, Delta.ThinkingDelta(t))   => b.copy(thinking = b.thinking + t)
          case (b: ContentBlock.Thinking, Delta.SignatureDelta(sg)) => b.copy(signature = b.signature + sg)
          case (other, _)                                           => other
        }
      }
    case StreamEvent.ContentBlockStop(index) =>
      partialJson.remove(index).foreach { sb =>
        val text = sb.toString
        // An empty input streams as no delta, or as an empty string.
        val input =
          if (text.trim.isEmpty) Json.obj()
          else Json.parse(text).getOrElse(Json.Str(text))
        updateBlock(index) {
          case b: ContentBlock.ToolUse       => b.copy(input = input)
          case b: ContentBlock.ServerToolUse => b.copy(input = input)
          case other                         => other
        }
      }
    case StreamEvent.MessageDelta(delta, usage) =>
      current = current.map { m =>
        val reason   = delta.get("stop_reason").flatMap(_.asString).map(StopReason(_)).orElse(m.stopReason)
        val sequence = delta.get("stop_sequence") match {
          case Some(s) => s.asString
          case None    => m.stopSequence
        }
        val details = delta.get("stop_details").filterNot(_.isNull).orElse(m.stopDetails)
        m.copy(stopReason = reason, stopSequence = sequence, stopDetails = details, usage = m.usage.merge(usage))
      }
    case _ => ()
  }
}

/** An open stream. Read it event by event (it is an `Iterator[StreamEvent]`, or call [[nextEvent]]), or
  * all at once with [[finalMessage]]; either way the message is accumulated as events arrive.
  *
  * Reading throws [[AnthropicException]]: an `error` event becomes an [[AnthropicException.ApiError]]
  * with no status. Close the stream (or read it to the end) to release the connection; it is an
  * `AutoCloseable`, so `scala.util.Using` works.
  */
final class MessageStream private[anthropic] (body: Http.Body, val requestId: Option[String])
    extends Iterator[StreamEvent]
    with AutoCloseable {

  private val parser             = new SseParser
  private val pending            = mutable.Queue.empty[String]
  private val acc                = new Accumulator
  private var ended              = false
  private var stopped            = false
  private var lookahead: Option[StreamEvent] = None
  private val chunk              = new Array[Byte](8192)

  /** The message as accumulated so far (`None` before `message_start`). */
  def snapshot: Option[Message] = acc.message

  /** The next event, `None` once the stream has ended. */
  def nextEvent(): Option[StreamEvent] = lookahead match {
    case some @ Some(_) =>
      lookahead = None
      some
    case None => read()
  }

  override def hasNext: Boolean = lookahead.isDefined || {
    lookahead = read()
    lookahead.isDefined
  }

  override def next(): StreamEvent = nextEvent().getOrElse(throw new NoSuchElementException("the stream has ended"))

  private def read(): Option[StreamEvent] = {
    while (pending.isEmpty && !ended) {
      val n =
        try body.read(chunk)
        catch {
          case e: AnthropicException =>
            ended = true
            close()
            throw e
        }
      if (n < 0) {
        ended = true
        pending ++= parser.finish()
        body.close()
      } else pending ++= parser.push(chunk, n)
    }
    if (pending.isEmpty) None
    else {
      val data  = pending.dequeue()
      val event = Decode.run("stream event")(StreamEvent.fromJson(Decode.parse(data, "stream event")))
      acc(event)
      event match {
        case StreamEvent.Error(error) =>
          ended = true
          pending.clear()
          close()
          throw AnthropicException.ApiError(None, error.kind, error.message, requestId)
        case StreamEvent.MessageStop => stopped = true
        case _                       => ()
      }
      Some(event)
    }
  }

  /** Reads the rest of the stream and returns the complete message. Fails if the stream reported an
    * error or ended before `message_stop`.
    */
  def finalMessage(): Message = {
    try while (nextEvent().isDefined) ()
    finally close()
    acc.message match {
      case Some(m) if stopped => m
      case _                  => throw AnthropicException.DecodeError("the stream ended before message_stop")
    }
  }

  /** Releases the connection; events not yet read are dropped. */
  override def close(): Unit = {
    ended = true
    body.close()
  }
}
