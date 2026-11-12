# anthropic-api-scala-sdk

A small typed Scala client for Anthropic's Messages API (`POST /v1/messages`) and the endpoints around it, over raw HTTPS, following the documented wire format. Cross-built for **Scala 3** and **Scala 2.13**, Java 17+. The core has **no dependencies**: it uses the JDK `HttpClient` and its own small JSON AST.

The Scala counterpart of [rust-claude-sdk](https://github.com/aoprisan/rust-claude-sdk), with the same features:

- Requests with system prompts (top-level or mid-conversation), `document` blocks with citations, images, tools, thinking and effort settings, task budgets, structured outputs (`json_schema`, read back with `Message.json`), `cache_control` breakpoints and server-side refusal `fallbacks` (the matching `anthropic-beta` headers are added automatically).
- Responses with text, citations, thinking, tool calls and fallback markers. Unknown block, citation, event and delta types are kept as raw JSON (`Other`) instead of failing, so a new API feature never breaks decoding and assistant turns echo back intact.
- Retries on connection errors, 408, 409, 429 and 5xx (honouring `retry-after` and `x-should-retry`), bounded by an optional deadline that also cuts retries short.
- Streaming over server-sent events, event by event (`MessageStream` is an `Iterator`) or accumulated into the final message.
- Server tools: web search, web fetch, code execution and tool search, with typed definitions, typed result blocks (errors included), web search citations and `usage.server_tool_use`.
- A tool-use loop, `AnthropicClient.runTools`: runs the tools the model calls (a turn's calls concurrently), sends all results back in one turn and resumes `pause_turn`, bounded by `maxRequests`.
- `POST /v1/messages/count_tokens`, Message Batches (results read line by line), the Files API and the Models API.
- A Cats Effect / fs2 module: every call as an `F`, cancelable; events, batch results and paginated lists as `fs2.Stream`s; an effectful tool loop.

## Installation

```scala
libraryDependencies += "io.github.aoprisan" %% "anthropic-api-scala-sdk" % "<version>"

// optional: Cats Effect 3 and fs2
libraryDependencies += "io.github.aoprisan" %% "anthropic-api-scala-sdk-cats-effect" % "<version>"
```

## Usage

```scala
import anthropic._

val client = AnthropicClient(ClientConfig.fromEnv().getOrElse(sys.error("ANTHROPIC_API_KEY")))
val request = MessagesRequest("claude-opus-5-5", 2048)
  .withSystemCached("Answer only from the documents and cite them.")
  .message(MessageParam.user(
    ContentBlockParam.textDocument("Cartea de identitate se eliberează în 30 de zile.", "Ghid CI").withCitations,
    ContentBlockParam.text("În cât timp primesc buletinul?")
  ))
val message = client.create(request)
if (!message.isRefusal) {
  println(message.text)
  message.citations.foreach { case (span, citation) => println(s"$span ← ${citation.citedTextOption}") }
}
```

Calls block the calling thread, which is cheap on virtual threads, and throw `AnthropicException`. It is a sealed hierarchy: `ApiError` (with `status`, `kind`, `message` and `requestId`), `TransportError`, `DecodeError`, `DeadlineExceeded`, `ConfigError` and `InvalidRequest`, and `isRetryable` says whether the same request may succeed later. Errors and `toString`s never show the credential.

`CallOptions.within(30.seconds)` puts a deadline on a whole call: every attempt, every wait between retries, and for a stream every read.

### Streaming

```scala
scala.util.Using.resource(client.stream(request)) { stream =>
  stream.foreach {
    case StreamEvent.ContentBlockDelta(_, Delta.TextDelta(text)) => print(text)
    case _                                                        => ()
  }
  stream.snapshot.foreach(m => println(s"\n${m.usage.outputTokens} tokens"))
}
val whole: Message = client.createStreamed(request) // stream, then accumulate
```

### Tools

The handler gets each call and returns a `ToolOutput`. An error output, or an exception the handler throws, goes back to the model as an error result. `run.request` holds the whole conversation, so a run that was cut short resumes when you call `runTools(run.request)`.

```scala
val withTool = MessagesRequest("claude-opus-5-5", 4096)
  .tool(Tool("get_weather", "Current weather for a city",
    Json.unsafeParse("""{"type": "object", "properties": {"city": {"type": "string"}}, "required": ["city"]}""")))
  .user("What's the weather in Cluj?")
val run = client.runTools(withTool) { call =>
  call.name match {
    case "get_weather" => ToolOutput.text(s"Sunny in ${call.input("city").asString.getOrElse("?")}")
    case other         => ToolOutput.error(s"unknown tool $other")
  }
}
if (run.isFinished && !run.message.isRefusal) println(run.message.text)
```

### Web search

```scala
val search = MessagesRequest("claude-opus-5-5", 4096)
  .tool(WebSearchTool().withMaxUses(3).withAllowedDomains("mai.gov.ro"))
  .user("Ce acte îmi trebuie pentru pașaport?")
val answer = client.create(search)
answer.citations.foreach { case (span, citation) => println(s"$span ← ${citation.urlOption}") }
answer.serverToolErrors.foreach { case (id, error) => Console.err.println(s"$id: ${error.errorCode}") }
```

### Batches, files and models

```scala
val batch = client.createBatch(Seq(BatchRequest("q-1", MessagesRequest("claude-opus-5-5", 1024).user("Salut"))))
client.awaitBatch(batch.id) // polls until it has ended
scala.util.Using.resource(client.batchResults(batch.id)) { results =>
  results.foreach {
    case BatchResult(id, BatchOutcome.Succeeded(message)) => println(s"$id: ${message.text}")
    case BatchResult(id, other)                           => println(s"$id: $other")
  }
}

val file = client.uploadFile(java.nio.file.Paths.get("ghid.pdf"), "application/pdf")
val aboutFile = MessagesRequest("claude-opus-5-5", 2048)
  .message(MessageParam.user(ContentBlockParam.fileDocument(file.id), ContentBlockParam.text("Rezumă.")))

val opus = client.model("claude-opus-5-5")
println(s"${opus.maxInputTokens} tokens of context, adaptive thinking: ${opus.supports("thinking", "types", "adaptive")}")
val page = client.listModels(ListParams.limit(20)) // page.nextParams(...) for the next page
```

### Cats Effect and fs2

```scala
import anthropic._
import anthropic.catseffect.AnthropicClientF
import cats.effect.IO

AnthropicClientF.fromEnv[IO].use { client =>
  client
    .stream(MessagesRequest("claude-opus-5-5", 1024).user("Salut!"))
    .through(AnthropicClientF.textDeltas)    // or AnthropicClientF.accumulate for the message
    .foreach(IO.print)
    .compile
    .drain
}
```

Cancelling a call aborts its HTTP exchange. Streams release their connection when they end, fail or are cancelled. `allModels`, `allFiles` and `allBatches` walk every page lazily. `runTools` takes a `ToolCall => F[ToolOutput]` handler and runs a turn's calls in parallel. `awaitBatch` sleeps on the runtime between polls.

### JSON

Tool inputs, schemas and other free-form fields are `anthropic.Json` values. Build them with `Json.obj(...)` / `Json.arr(...)` or parse them with `Json.parse`. Read them with `get`, `apply`, `at`, `asString`, `asLong` and the other accessors. To use another JSON library, convert through text: `Json.parse(circeJson.noSpaces)` in, and `myJson.render` out.

### API version

Requests go out with `anthropic-version: 2026-10-21` ([migration guide](https://platform.claude.com/docs/en/api/migration-2026-10-21)). On that version:

- `temperature`, `top_p` and `top_k` are removed. A `document` block directly in an assistant message is rejected, and so is `citations` on text in a user message (`search_result` blocks included). `MessagesRequest.validate` checks these rules, and the client runs it before sending, so such a request fails with `AnthropicException.InvalidRequest` and is never sent.
- `MessageParam` has one case per role (`User`, `Assistant`, `System`). A system message holds text only. `role` and `content` read a message of any role.
- An API key is sent as `Authorization: Bearer`. For a proxy or gateway that reads `x-api-key`, use `Credential.XApiKey`.

To keep the old behaviour for a client, for example to sample with a lower `temperature` on a model that accepts it, set `apiVersion = AnthropicClient.LegacyApiVersion` in its `ClientConfig`. Requests on that version are not checked.

## Examples

Runnable programs in [`examples/`](examples/src/main/scala/examples), each started with `ANTHROPIC_API_KEY=... sbt "examples/runMain examples.<Name> [args]"`:

| Example | Shows |
| --- | --- |
| [`Grounded`](examples/src/main/scala/examples/Grounded.scala) | A streamed answer grounded in documents, with citations, prompt caching, effort and refusal fallbacks |
| [`Chat`](examples/src/main/scala/examples/Chat.scala) | A multi-turn terminal chat: streaming, adaptive thinking shown live, echoing assistant turns back |
| [`Tools`](examples/src/main/scala/examples/Tools.scala) | The tool-use loop with two local tools (one strict), concurrent calls and a deadline |
| [`WebSearch`](examples/src/main/scala/examples/WebSearch.scala) | Web search and web fetch server tools, cited sources, server tool errors and usage |
| [`CodeExecution`](examples/src/main/scala/examples/CodeExecution.scala) | Server-side code execution, its output, and downloading the files it writes |
| [`FilesApi`](examples/src/main/scala/examples/FilesApi.scala) | Uploading a PDF to the Files API, asking about it by id with citations, deleting it |
| [`Batch`](examples/src/main/scala/examples/Batch.scala) | Creating a Message Batch, polling it and reading its results |
| [`Models`](examples/src/main/scala/examples/Models.scala) | Paging through the Models API with capabilities, and counting tokens |
| [`StreamingIO`](examples/src/main/scala/examples/StreamingIO.scala) | The Cats Effect binding: an fs2 text stream, parallel calls, every model across pages |

## Development

sbt 1.11 and a JDK 17 or later.

```sh
sbt +test           # every module on Scala 3 and Scala 2.13
sbt +publishLocal   # the jars, sources, Scaladoc and POMs of a release
```

Tests run against a local stand-in for the API and need no API key. One source tree serves both Scala versions: Scala 2.13 compiles it with `-Xsource:3`, and warnings are errors on both.

## Releasing

Push a tag `vX.Y.Z` and the `release` workflow publishes `X.Y.Z` to Maven Central for Scala 3 and 2.13; every push to `main` publishes a snapshot. See [RELEASING.md](RELEASING.md) for the one-time setup (namespace, user token, PGP key, repository secrets).

## License

[MIT](LICENSE).
