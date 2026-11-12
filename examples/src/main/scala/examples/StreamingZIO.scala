package examples

import anthropic._
import anthropic.zio2.AnthropicClientZIO
import zio.{Console, ZIO, ZIOAppDefault}

/** The ZIO binding: a streamed answer printed as it arrives, two questions asked in parallel, and every
  * model listed as one `ZStream` across pages. Errors are `AnthropicException`s in the typed channel.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.StreamingZIO"
  * }}}
  */
object StreamingZIO extends ZIOAppDefault {
  private val ask = (q: String) => MessagesRequest("claude-opus-5-5", 1024).withEffort(Effort.Low).user(q)

  private val program: ZIO[AnthropicClientZIO, Exception, Unit] =
    ZIO.serviceWithZIO[AnthropicClientZIO] { client =>
      for {
        _ <- client
          .stream(ask("Explain in three sentences what a CNP is."))
          .via(AnthropicClientZIO.textDeltas)
          .runForeach(t => Console.print(t))
        _       <- Console.printLine("\n")
        answers <- ZIO.foreachPar(List("Capital of Romania?", "Longest Romanian river?"))(q => client.create(ask(q)))
        _       <- ZIO.foreachDiscard(answers)(m => Console.printLine(m.text))
        ids     <- client.allModels().map(_.id).runCollect
        _       <- Console.printLine(s"\n${ids.length} models: ${ids.mkString(", ")}")
      } yield ()
    }

  def run: ZIO[Any, Exception, Unit] = program.provide(AnthropicClientZIO.live)
}
