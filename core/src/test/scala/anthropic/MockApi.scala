package anthropic

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** A canned reply: `delay` before anything is sent, `stall` after the body before the response ends. */
final case class Reply(
    status: Int,
    body: String,
    headers: Seq[(String, String)] = Nil,
    delay: FiniteDuration = Duration.Zero,
    stall: FiniteDuration = Duration.Zero
) {
  def withHeaders(h: (String, String)*): Reply = copy(headers = headers ++ h)
}

object Reply {
  def ok(body: String): Reply = Reply(200, body)
}

/** One request the stand-in received. Header names are lower-cased. */
final case class Seen(method: String, path: String, headers: Map[String, String], raw: String) {
  def header(name: String): Option[String] = headers.get(name.toLowerCase)
  def json: Json                           = Json.parse(raw).getOrElse(Json.Null)
  def route: String                        = s"$method $path"
}

/** A local stand-in for the API: answers the `n`-th request with the `n`-th reply (the last one after
  * that) and records every request.
  */
final class MockApi(replies: Seq[Reply]) extends AutoCloseable {
  private val hitCount = new AtomicInteger(0)
  private val received = new ConcurrentLinkedQueue[Seen]()
  private val server   = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  server.setExecutor(Executors.newCachedThreadPool { (r: Runnable) =>
    val t = new Thread(r); t.setDaemon(true); t
  })
  server.createContext(
    "/",
    (ex: HttpExchange) => {
      val n = hitCount.getAndIncrement()
      val headers = ex.getRequestHeaders.asScala.map { case (k, v) => k.toLowerCase -> v.asScala.mkString(",") }.toMap
      val raw     = new String(ex.getRequestBody.readAllBytes(), UTF_8)
      val path    = ex.getRequestURI.getRawPath + Option(ex.getRequestURI.getRawQuery).fold("")("?" + _)
      received.add(Seen(ex.getRequestMethod, path, headers, raw))
      val r = replies.lift(n).getOrElse(replies.last)
      try {
        if (r.delay > Duration.Zero) Thread.sleep(r.delay.toMillis)
        r.headers.foreach { case (k, v) => ex.getResponseHeaders.add(k, v) }
        val bytes = r.body.getBytes(UTF_8)
        if (r.stall > Duration.Zero) {
          ex.sendResponseHeaders(r.status, 0) // chunked
          ex.getResponseBody.write(bytes)
          ex.getResponseBody.flush()
          Thread.sleep(r.stall.toMillis)
        } else {
          ex.sendResponseHeaders(r.status, if (bytes.isEmpty) -1L else bytes.length.toLong)
          if (bytes.nonEmpty) ex.getResponseBody.write(bytes)
        }
      } catch { case _: Exception => () }
      finally ex.close()
    }
  )
  server.start()

  val url: String = s"http://127.0.0.1:${server.getAddress.getPort}"

  def hits: Int        = hitCount.get
  def seen: Vector[Seen] = received.asScala.toVector

  override def close(): Unit = server.stop(0)
}

object MockApi {
  val Key = "sk-ant-test-SECRET"

  def client(api: MockApi, maxRetries: Int = 0): AnthropicClient =
    AnthropicClient(ClientConfig.apiKey(Key).copy(baseUrl = api.url + "/", maxRetries = maxRetries))

  /** A server-sent event stream of the given events. */
  def sse(events: Json*): String =
    events.map(e => s"event: ${e("type").asString.getOrElse("")}\ndata: ${e.render}\n\n").mkString

  def json(s: String): Json = Json.unsafeParse(s)
}
