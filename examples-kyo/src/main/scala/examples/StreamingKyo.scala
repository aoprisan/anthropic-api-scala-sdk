package examples

import anthropic.*
import anthropic.kyoeffect.AnthropicClientKyo
import anthropic.kyoeffect.AnthropicClientKyo.textDeltas
import kyo.*

/** The Kyo binding: a streamed answer printed as it arrives, two questions asked in parallel, and every
  * model listed as one `Stream` across pages. Errors are `AnthropicException`s in `Abort`. Needs JDK 25.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examplesKyo/runMain examples.StreamingKyo"
  * }}}
  */
object StreamingKyo extends KyoApp {
  private val ask = (q: String) => MessagesRequest("claude-opus-5-5", 1024).withEffort(Effort.Low).user(q)

  run {
    AnthropicClientKyo.fromEnv.map { client =>
      for {
        _       <- client.stream(ask("Explain in three sentences what a CNP is.")).textDeltas.foreach(Console.print(_))
        _       <- Console.printLine("\n")
        answers <- Async.foreach(List("Capital of Romania?", "Longest Romanian river?"))(q => client.create(ask(q)))
        _       <- Kyo.foreachDiscard(answers)(m => Console.printLine(m.text))
        ids     <- client.allModels().map(_.id).run
        _       <- Console.printLine(s"\n${ids.length} models: ${ids.mkString(", ")}")
      } yield ()
    }
  }
}
