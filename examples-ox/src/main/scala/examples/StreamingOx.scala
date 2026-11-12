package examples

import anthropic.*
import anthropic.oxflow.*
import ox.{par, supervised, useCloseableInScope}

/** The Ox binding: a streamed answer printed as it arrives, two questions asked in parallel, and every
  * model listed as one `Flow` across pages. Needs JDK 21.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examplesOx/runMain examples.StreamingOx"
  * }}}
  */
object StreamingOx {
  def main(args: Array[String]): Unit = supervised {
    val client = useCloseableInScope(AnthropicClient.fromEnv())
    val ask    = (q: String) => MessagesRequest("claude-opus-5-5", 1024).withEffort(Effort.Low).user(q)

    client.streamFlow(ask("Explain in three sentences what a CNP is.")).textDeltas.runForeach(print)
    println("\n")

    val (capital, river) = par(client.create(ask("Capital of Romania?")), client.create(ask("Longest Romanian river?")))
    println(capital.text)
    println(river.text)

    val ids = client.allModels().map(_.id).runToList()
    println(s"\n${ids.length} models: ${ids.mkString(", ")}")
  }
}
