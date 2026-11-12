default: test

# every module on Scala 3 and Scala 2.13
test:
    sbt +test

compile:
    sbt +compile

# the jars, sources, Scaladoc and POMs of a release, into ~/.ivy2/local
publish-local:
    sbt +publishLocal

# run every sample in examples/ (needs ANTHROPIC_API_KEY)
examples:
    sbt "examples/runMain examples.Grounded" \
        "examples/runMain examples.Tools" \
        "examples/runMain examples.WebSearch" \
        "examples/runMain examples.CodeExecution" \
        "examples/runMain examples.Batch" \
        "examples/runMain examples.Models" \
        "examples/runMain examples.StreamingIO" \
        "examples/runMain examples.StreamingZIO" \
        "examplesMonix/runMain examples.StreamingMonix"

# interactive chat in the terminal (needs ANTHROPIC_API_KEY)
chat:
    sbt "examples/runMain examples.Chat"
