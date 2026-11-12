package examples

import anthropic._
import scala.io.StdIn
import scala.util.Using

/** A multi-turn chat in the terminal: streamed answers, adaptive thinking shown as it happens, and the
  * conversation kept in the request so each turn sees the earlier ones.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.Chat"
  * }}}
  */
object Chat {
  def main(args: Array[String]): Unit = Using.resource(AnthropicClient.fromEnv()) { client =>
    var request = MessagesRequest("claude-opus-5-5", 8000)
      .withSystemCached("You are a concise, friendly assistant.")
      .withThinking(ThinkingConfig.Adaptive(Some(ThinkingDisplay.Summarized)))
      .withEffort(Effort.Medium)

    println("Type a message, an empty line to quit.")
    var line = { print("\n> "); StdIn.readLine() }
    while (line != null && line.trim.nonEmpty) {
      val asked  = request.user(line.trim)
      val stream = client.stream(asked)
      var thinking = false
      stream.foreach {
        case StreamEvent.ContentBlockDelta(_, Delta.ThinkingDelta(t)) =>
          if (!thinking) { print("\u001b[2m(thinking) "); thinking = true }
          print(t); Console.flush()
        case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(t)) =>
          if (thinking) { print("\u001b[0m\n\n"); thinking = false }
          print(t); Console.flush()
        case _ => ()
      }
      if (thinking) print("\u001b[0m")
      println()

      val message = stream.finalMessage()
      // A refused turn is not echoed back; the question is dropped so the chat can go on.
      if (message.isRefusal) println("(declined)")
      // Echo the whole turn, thinking blocks included, as the next request's context.
      else request = asked.message(MessageParam.assistant(message))
      line = { print("\n> "); StdIn.readLine() }
    }
  }
}
