package examples

import anthropic._
import scala.util.Using

/** Server tools: the API searches the web and fetches pages itself, and the answer cites the pages it used.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.WebSearch What documents do I need for a Romanian passport?"
  * }}}
  */
object WebSearch {
  def main(args: Array[String]): Unit = {
    val question = if (args.nonEmpty) args.mkString(" ") else "What documents do I need for a Romanian passport?"
    Using.resource(AnthropicClient.fromEnv()) { client =>
      val request = MessagesRequest("claude-opus-5-5", 8000)
        .withSystem("Answer from official sources only and cite them.")
        .tool(WebSearchTool().withMaxUses(5).withAllowedDomains("gov.ro", "mai.gov.ro"))
        .tool(
          WebFetchTool().withMaxUses(3).withAllowedDomains("gov.ro", "mai.gov.ro").withCitations.withMaxContentTokens(20000)
        )
        .user(question)

      // Server tools run inside the API, so there are no local calls to handle; the loop is still useful
      // because it resumes `pause_turn` when the server's own loop runs long.
      val run     = client.runTools(request, ToolLoopOptions(stream = true))(call => ToolOutput.error(s"no local tool ${call.name}"))
      val message = run.message
      if (message.isRefusal) println("(declined)")
      else {
        println(s"${message.text}\n")
        println("Sources:")
        message.citations.flatMap(_._2.urlOption).distinct.foreach(url => println(s"  $url"))
        message.serverToolErrors.foreach { case (id, error) =>
          Console.err.println(s"server tool call $id failed: ${error.errorCode}")
        }
        run.usage.serverToolUse.foreach(used =>
          println(s"\n${used.webSearchRequests} searches, ${used.webFetchRequests} fetches")
        )
      }
    }
  }
}
