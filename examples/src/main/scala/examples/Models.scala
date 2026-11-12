package examples

import anthropic._
import scala.util.Using

/** The Models API and token counting: list every model available to the key with its limits and
  * capabilities, then count a prompt's tokens before sending it.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.Models"
  * }}}
  */
object Models {
  def main(args: Array[String]): Unit = Using.resource(AnthropicClient.fromEnv()) { client =>
    // Walk every page.
    var params: Option[ListParams] = Some(ListParams.limit(20))
    while (params.isDefined) {
      val page = client.listModels(params.get)
      page.data.foreach { m =>
        println(
          f"${m.id}%-32s ${m.displayName}%-24s context ${m.maxInputTokens.fold("?")(_.toString)}%9s  " +
            f"output ${m.maxTokens.fold("?")(_.toString)}%7s  images ${m.supports("image_input")}%-5s " +
            s"adaptive thinking ${m.supports("thinking", "types", "adaptive")}"
        )
      }
      params = page.nextParams(params.get)
    }

    val model = client.model("claude-opus-5-5")
    val request = MessagesRequest(model.id, 1024)
      .withSystem("You are a helpful assistant.")
      .user("Explain the difference between a buletin and a carte electronică de identitate.")
    println(s"\nthat prompt is ${client.countTokens(request)} input tokens on ${model.displayName}")
  }
}
