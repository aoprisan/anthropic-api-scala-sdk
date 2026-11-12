package examples

import anthropic._
import scala.concurrent.duration._
import scala.util.Using

/** The tool-use loop: two local tools the model calls as it needs them, run concurrently when it asks for
  * several at once, with their results sent back until it answers.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.Tools What's the weather in Cluj and Iași?"
  * }}}
  */
object Tools {

  /** A stand-in for a real weather service. */
  def weather(city: String): Either[String, String] = {
    Thread.sleep(200)
    city.toLowerCase match {
      case "cluj" | "cluj-napoca"                  => Right("14°C, cloudy")
      case "iași" | "iasi"                         => Right("17°C, sunny")
      case "bucurești" | "bucuresti" | "bucharest" => Right("19°C, sunny")
      case _                                       => Left(s"no weather station in $city")
    }
  }

  def calculate(op: String, a: Double, b: Double): Either[String, String] = op match {
    case "add"                => Right((a + b).toString)
    case "subtract"           => Right((a - b).toString)
    case "multiply"           => Right((a * b).toString)
    case "divide" if b != 0.0 => Right((a / b).toString)
    case "divide"             => Left("division by zero")
    case other                => Left(s"unknown operation $other")
  }

  def handle(call: ToolCall): ToolOutput = {
    println(s"  → ${call.name}(${call.input.render})")
    def str(k: String) = call.input(k).asString.getOrElse("")
    def num(k: String) = call.input(k).asDouble.getOrElse(0.0)
    call.name match {
      case "get_weather" => ToolOutput.fromEither(weather(str("city")))
      case "calculate"   => ToolOutput.fromEither(calculate(str("operation"), num("a"), num("b")))
      case other         => ToolOutput.error(s"unknown tool $other")
    }
  }

  def main(args: Array[String]): Unit = {
    val question =
      if (args.nonEmpty) args.mkString(" ")
      else "What's the weather in Cluj and Iași, and how much warmer is the warmer one?"
    Using.resource(AnthropicClient.fromEnv()) { client =>
      val request = MessagesRequest("claude-opus-5-5", 4096)
        .withSystem("Use the tools for weather and arithmetic; do not guess.")
        .tool(
          Tool(
            "get_weather",
            "Current weather for a Romanian city.",
            Json.unsafeParse("""{"type": "object", "properties": {"city": {"type": "string"}}, "required": ["city"]}""")
          )
        )
        .tool(
          Tool(
            "calculate",
            "Exact arithmetic on two numbers.",
            Json.unsafeParse("""{
              "type": "object",
              "properties": {
                "operation": {"type": "string", "enum": ["add", "subtract", "multiply", "divide"]},
                "a": {"type": "number"},
                "b": {"type": "number"}
              },
              "required": ["operation", "a", "b"],
              "additionalProperties": false
            }""")
          ).withStrict
        )
        .user(question)

      val options = ToolLoopOptions(maxRequests = 10, call = CallOptions.within(120.seconds))
      val run     = client.runTools(request, options)(handle)

      if (!run.isFinished) println(s"(stopped after ${run.requests} requests; runTools on run.request would carry on)")
      else if (run.message.isRefusal) println("(declined)")
      else println(s"\n${run.message.text}")
      println(s"\n${run.requests} requests, ${run.usage.inputTokens} tokens in, ${run.usage.outputTokens} out")
    }
  }
}
