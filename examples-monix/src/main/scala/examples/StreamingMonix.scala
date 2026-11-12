package examples

import anthropic._
import anthropic.monixtask.AnthropicClientTask
import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}

/** The Monix binding: a streamed answer printed as it arrives, two questions asked in parallel, and
  * every model listed as one `Observable` across pages.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examplesMonix/runMain examples.StreamingMonix"
  * }}}
  */
object StreamingMonix extends TaskApp {
  def run(args: List[String]): Task[ExitCode] = AnthropicClientTask.fromEnv.use { client =>
    val ask = (q: String) => MessagesRequest("claude-opus-5-5", 1024).withEffort(Effort.Low).user(q)
    for {
      _ <- client
        .stream(ask("Explain in three sentences what a CNP is."))
        .transform(AnthropicClientTask.textDeltas)
        .mapEval(t => Task(print(t)))
        .completedL
      _       <- Task(println("\n"))
      answers <- Task.parTraverse(List("Capital of Romania?", "Longest Romanian river?"))(q => client.create(ask(q)))
      _       <- Task(answers.foreach(m => println(m.text)))
      ids     <- client.allModels().map(_.id).toListL
      _       <- Task(println(s"\n${ids.length} models: ${ids.mkString(", ")}"))
    } yield ExitCode.Success
  }
}
