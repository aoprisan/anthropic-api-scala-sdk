package examples

import anthropic._
import anthropic.catseffect.AnthropicClientF
import cats.effect.{IO, IOApp}
import cats.syntax.all._

/** The Cats Effect binding: a streamed answer printed as it arrives, two questions asked in parallel, and
  * every model listed as one fs2 stream across pages.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.StreamingIO"
  * }}}
  */
object StreamingIO extends IOApp.Simple {
  def run: IO[Unit] = AnthropicClientF.fromEnv[IO].use { client =>
    val ask = (q: String) => MessagesRequest("claude-opus-5-5", 1024).withEffort(Effort.Low).user(q)
    for {
      _ <- client
        .stream(ask("Explain in three sentences what a CNP is."))
        .through(AnthropicClientF.textDeltas)
        .foreach(t => IO.print(t))
        .compile
        .drain
      _ <- IO.println("\n")
      answers <- List("Capital of Romania?", "Longest Romanian river?").parTraverse(q => client.create(ask(q)))
      _       <- answers.traverse_(m => IO.println(m.text))
      ids     <- client.allModels().map(_.id).compile.toList
      _       <- IO.println(s"\n${ids.length} models: ${ids.mkString(", ")}")
    } yield ()
  }
}
