package examples

import anthropic._
import java.nio.file.{Files, Paths}
import scala.util.Using

/** Server-side code execution: the model writes and runs code in a sandbox, and any files it writes are
  * downloaded with the Files API.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.CodeExecution"
  * }}}
  */
object CodeExecution {
  def main(args: Array[String]): Unit = {
    val config = ClientConfig.fromEnv().getOrElse(sys.error("set ANTHROPIC_API_KEY (or ANTHROPIC_AUTH_TOKEN)"))
    // Downloading the files the sandbox writes uses the Files API.
    Using.resource(AnthropicClient(config.copy(betas = config.betas :+ "files-api-2025-04-14"))) { client =>
      val request = MessagesRequest("claude-opus-5-5", 8000)
        .tool(CodeExecutionTool())
        .user(
          "Compute the first 30 Fibonacci numbers, report how many are prime, " +
            "and save them with a primality column to fibonacci.csv."
        )
      val message = client.create(request)
      if (message.isRefusal) println("(declined)")
      else {
        val files = message.content.flatMap {
          case ContentBlock.ServerToolUse(_, name, input, _) =>
            println(s"$$ $name: ${input.get("command").getOrElse(input).render}")
            Vector.empty
          case ContentBlock.BashCodeExecutionToolResult(_, CodeExecutionContent.Output(out), _) =>
            print(out.stdout)
            Console.err.print(out.stderr)
            if (out.returnCode != 0) println(s"(exit ${out.returnCode})")
            out.fileIds
          case ContentBlock.Text(text, _) =>
            println(text)
            Vector.empty
          case _ => Vector.empty
        }
        message.serverToolErrors.foreach { case (id, error) =>
          Console.err.println(s"code execution $id failed: ${error.errorCode}")
        }
        files.foreach { id =>
          val meta  = client.file(id)
          val bytes = client.downloadFile(id)
          val name  = Paths.get(meta.filename).getFileName
          Files.write(name, bytes)
          println(s"saved $name (${bytes.length} bytes)")
        }
      }
    }
  }
}
