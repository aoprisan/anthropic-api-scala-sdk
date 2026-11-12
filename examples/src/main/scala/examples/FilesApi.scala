package examples

import anthropic._
import java.nio.file.Paths
import scala.util.{Try, Using}

/** The Files API: upload a PDF once, ask about it by id, then delete it.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.FilesApi path/to/document.pdf Summarise it in five bullet points."
  * }}}
  */
object FilesApi {
  def main(args: Array[String]): Unit = {
    val path     = Paths.get(args.headOption.getOrElse(sys.error("usage: FilesApi <document.pdf> [question]")))
    val question = if (args.length > 1) args.drop(1).mkString(" ") else "Summarise it in five bullet points."
    val config   = ClientConfig.fromEnv().getOrElse(sys.error("set ANTHROPIC_API_KEY (or ANTHROPIC_AUTH_TOKEN)"))
    Using.resource(AnthropicClient(config.copy(betas = config.betas :+ "files-api-2025-04-14"))) { client =>
      val file = client.uploadFile(path, "application/pdf")
      println(s"uploaded ${file.filename} as ${file.id} (${file.sizeBytes} bytes)")

      val request = MessagesRequest("claude-opus-5-5", 4096)
        .message(MessageParam.user(ContentBlockParam.fileDocument(file.id).withCitations, ContentBlockParam.text(question)))
      val answer = Try(client.create(request))

      // Clean up whether or not the question succeeded.
      client.deleteFile(file.id)
      println(s"deleted ${file.id}")
      println(s"${client.listFiles(ListParams.limit(5)).data.length} recent files left in the workspace\n")

      val message = answer.get
      if (message.isRefusal) println("(declined)")
      else {
        println(message.text)
        message.citations.foreach { case (span, citation) =>
          println(s"  «$span» ← ${citation.citedTextOption.getOrElse("")}")
        }
      }
    }
  }
}
