package examples

import anthropic._
import scala.concurrent.duration._
import scala.util.Using

/** Message Batches: many requests at half price, processed asynchronously. Creates a batch, polls until it
  * ends, then reads the results as they download.
  *
  * {{{
  * ANTHROPIC_API_KEY=... sbt "examples/runMain examples.Batch"
  * }}}
  */
object Batch {
  def main(args: Array[String]): Unit = Using.resource(AnthropicClient.fromEnv()) { client =>
    val words = Vector("primărie", "buletin", "pașaport", "permis de conducere")
    val requests = words.zipWithIndex.map { case (w, i) =>
      val params = MessagesRequest("claude-haiku-4-5", 200)
        .withSystem("Translate the Romanian term to English and explain it in one sentence.")
        .user(w)
      BatchRequest(s"term-$i", params)
    }

    val created = client.createBatch(requests)
    println(s"created ${created.id}")
    val batch = client.awaitBatch(created.id, pollEvery = 15.seconds)
    val c     = batch.requestCounts
    println(s"${batch.processingStatus.value}: ${c.succeeded} succeeded, ${c.errored} errored")

    // Results come in any order; match them back by `customId`.
    Using.resource(client.batchResults(batch.id)) { results =>
      results.foreach { result =>
        val word = words(result.customId.stripPrefix("term-").toInt)
        result.result match {
          case BatchOutcome.Succeeded(message) => println(s"\n$word: ${message.text}")
          case errored: BatchOutcome.Errored   => println(s"\n$word: error ${errored.error.map(_.message)}")
          case other                           => println(s"\n$word: $other")
        }
      }
    }
  }
}
