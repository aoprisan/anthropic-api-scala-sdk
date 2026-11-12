package anthropic

import java.io.{IOException, InputStream}
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{Executors, ScheduledFuture, ScheduledThreadPoolExecutor, ThreadFactory, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.Deadline

/** Transport pieces shared by the client: URLs, request bodies, and response bodies read under a deadline. */
private[anthropic] object Http {

  /** Closes response bodies whose deadline passed, which wakes a reader blocked on them. */
  private lazy val watchdog: ScheduledThreadPoolExecutor = {
    val factory: ThreadFactory = { (r: Runnable) =>
      val t = Executors.defaultThreadFactory().newThread(r)
      t.setName("anthropic-sdk-deadline")
      t.setDaemon(true)
      t
    }
    val pool = new ScheduledThreadPoolExecutor(1, factory)
    pool.setRemoveOnCancelPolicy(true)
    pool
  }

  /** A response body. Reads fail with [[AnthropicException.DeadlineExceeded]] once the deadline set by
    * [[watch]] passes (the body is closed then, so a blocked read returns), and with
    * [[AnthropicException.TransportError]] when the connection breaks.
    */
  final class Body(in: InputStream) extends AutoCloseable {
    private val expired                         = new AtomicBoolean(false)
    private val closed                          = new AtomicBoolean(false)
    @volatile private var timer: ScheduledFuture[?] = null

    /** Bounds every later read by `deadline` (replacing any earlier bound). */
    def watch(deadline: Option[Deadline]): Body = {
      cancelTimer()
      deadline.foreach { d =>
        val left = d.timeLeft.toMillis
        if (left <= 0) expire()
        else timer = watchdog.schedule((() => expire()): Runnable, left, TimeUnit.MILLISECONDS)
      }
      this
    }

    private def expire(): Unit = {
      expired.set(true)
      quietlyClose()
    }

    private def cancelTimer(): Unit = {
      val t = timer
      if (t != null) t.cancel(false)
      timer = null
    }

    private def quietlyClose(): Unit =
      if (closed.compareAndSet(false, true)) {
        try in.close()
        catch { case _: IOException => () }
      }

    private def failure(e: Throwable): AnthropicException =
      if (expired.get) AnthropicException.DeadlineExceeded()
      else AnthropicException.TransportError(Option(e.getMessage).getOrElse(e.getClass.getSimpleName), Some(e))

    /** Like `InputStream.read`: bytes read, or -1 at the end. */
    def read(buf: Array[Byte]): Int = {
      if (expired.get) throw AnthropicException.DeadlineExceeded()
      try {
        val n = in.read(buf)
        if (expired.get) throw AnthropicException.DeadlineExceeded()
        n
      } catch {
        case e: AnthropicException => throw e
        case e: IOException        => throw failure(e)
      }
    }

    /** The rest of the body, then closes it. */
    def readAll(limit: Int = Int.MaxValue): Array[Byte] = {
      val out = new java.io.ByteArrayOutputStream
      val buf = new Array[Byte](8192)
      try {
        var n = read(buf)
        while (n >= 0 && out.size < limit) {
          out.write(buf, 0, n)
          n = read(buf)
        }
      } finally close()
      out.toByteArray
    }

    override def close(): Unit = {
      cancelTimer()
      quietlyClose()
    }
  }

  /** `baseUrl` with `segments` appended, each percent-encoded (an id cannot escape its segment), and the
    * query.
    */
  def url(baseUrl: String, segments: Seq[String], query: Seq[(String, String)]): URI = {
    val base =
      try URI.create(baseUrl.trim)
      catch { case e: IllegalArgumentException => throw AnthropicException.ConfigError(s"baseUrl: ${e.getMessage}") }
    if (base.getScheme == null || base.getHost == null || !Set("http", "https")(base.getScheme.toLowerCase))
      throw AnthropicException.ConfigError(s"baseUrl must be an absolute http(s) URL, got `$baseUrl`")
    if (base.getRawQuery != null || base.getRawFragment != null)
      throw AnthropicException.ConfigError("baseUrl cannot have a query or a fragment")
    val root = Option(base.getRawPath).getOrElse("").replaceAll("/+$", "")
    val path = root + segments.map(s => "/" + encodeSegment(s)).mkString
    val q =
      if (query.isEmpty) ""
      else query.map { case (k, v) => s"${URLEncoder.encode(k, "UTF-8")}=${URLEncoder.encode(v, "UTF-8")}" }.mkString("?", "&", "")
    val authority = base.getRawAuthority
    URI.create(s"${base.getScheme}://$authority$path$q")
  }

  private val keep: Set[Char] =
    (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet ++ "-._~!$&'()*+,;=:@".toSet

  def encodeSegment(segment: String): String =
    if (segment == "." || segment == "..") segment.replace(".", "%2E")
    else {
      val sb = new StringBuilder
      segment.getBytes(UTF_8).foreach { b =>
        val c = (b & 0xff).toChar
        if (b >= 0 && keep(c)) sb.append(c) else sb.append("%%%02X".format(b & 0xff))
      }
      sb.toString
    }

  /** A `multipart/form-data` body with one `file` field: its content type, and its bytes. */
  def multipart(filename: String, mimeType: String, data: Array[Byte]): (String, Array[Byte]) = {
    if (mimeType.exists(c => c == '\r' || c == '\n') || mimeType.trim.isEmpty)
      throw AnthropicException.ConfigError(s"mime type: invalid `$mimeType`")
    val boundary = "anthropic-scala-" + java.util.UUID.randomUUID().toString.replace("-", "")
    val name     = filename.replace("\"", "%22").replace("\r", "").replace("\n", "")
    val head =
      s"--$boundary\r\nContent-Disposition: form-data; name=\"file\"; filename=\"$name\"\r\nContent-Type: $mimeType\r\n\r\n"
    val tail = s"\r\n--$boundary--\r\n"
    val out  = new java.io.ByteArrayOutputStream(head.length + data.length + tail.length)
    out.write(head.getBytes(UTF_8))
    out.write(data)
    out.write(tail.getBytes(UTF_8))
    (s"multipart/form-data; boundary=$boundary", out.toByteArray)
  }
}
