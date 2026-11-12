default: test

# every module on each Scala version it supports (all of them only on JDK 25)
test:
    sbt +test

compile:
    sbt +compile

# the jars, sources, Scaladoc and POMs of a release, into ~/.ivy2/local
publish-local:
    sbt +publishLocal

# run every sample in examples/ (needs ANTHROPIC_API_KEY and JDK 25)
examples:
    sbt "examples/runMain examples.Grounded" \
        "examples/runMain examples.Tools" \
        "examples/runMain examples.WebSearch" \
        "examples/runMain examples.CodeExecution" \
        "examples/runMain examples.Batch" \
        "examples/runMain examples.Models" \
        "examples/runMain examples.StreamingIO" \
        "examples/runMain examples.StreamingZIO" \
        "examplesMonix/runMain examples.StreamingMonix" \
        "examplesOx/runMain examples.StreamingOx" \
        "examplesKyo/runMain examples.StreamingKyo"

# interactive chat in the terminal (needs ANTHROPIC_API_KEY)
chat:
    sbt "examples/runMain examples.Chat"
