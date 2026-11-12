// Publishing to Maven Central (Sonatype Central Portal).
// Brings in sbt-dynver (version from git tags) and sbt-pgp (signing).
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.12.1")

// Generates `anthropic.BuildInfo` in core, so the SDK reports the version it was built as.
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.2")
