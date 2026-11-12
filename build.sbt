val scala213 = "2.13.18"
val scala3   = "3.3.6"

ThisBuild / scalaVersion       := scala3
ThisBuild / crossScalaVersions := Seq(scala3, scala213)
ThisBuild / organization       := "io.github.aoprisan"
// Early semver: before 1.0 a minor bump (0.1 -> 0.2) may break binary compatibility, a patch may not.
// Written into the POMs, so sbt and other tools can tell which upgrades are safe.
ThisBuild / versionScheme := Some("early-semver")
// version is derived from git tags by sbt-dynver (see project/plugins.sbt):
// a `vX.Y.Z` tag releases X.Y.Z, anything else publishes a snapshot.

ThisBuild / homepage := Some(url("https://github.com/aoprisan/anthropic-api-scala-sdk"))
ThisBuild / licenses := List("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/aoprisan/anthropic-api-scala-sdk"),
    "scm:git@github.com:aoprisan/anthropic-api-scala-sdk.git"
  )
)
ThisBuild / developers := List(
  Developer("aoprisan", "Andrei", "", url("https://github.com/aoprisan"))
)

// Every module targets this JDK.
val baseRelease = "17"

val munitVersion           = "1.1.1"
val catsEffectVersion      = "3.7.1"
val fs2Version             = "3.14.0"
val munitCatsEffectVersion = "2.2.1"

/** One source tree for both Scala versions: Scala 2.13 reads it with `-Xsource:3`. */
def commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-release", baseRelease) ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("-Xsource:3", "-Wunused:imports,privates,locals,patvars", "-Werror")
      case _            => Seq("-Wunused:all", "-Werror")
    }
  },
  // Scaladoc runs the same options; don't fail the release jar on a doc warning.
  Compile / doc / scalacOptions --= Seq("-Werror", "-Xfatal-warnings"),
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
  Test / fork              := true,
  Test / parallelExecution := false
)

lazy val root = project
  .in(file("."))
  .aggregate(core, catsEffect, examples)
  .settings(
    name           := "anthropic-api-scala-sdk-root",
    publish / skip := true
  )

lazy val core = project
  .in(file("core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    name        := "anthropic-api-scala-sdk",
    description := "Zero-dependency Scala client for Anthropic's Messages API: citations, prompt caching, " +
      "refusal fallbacks, server tools, retries, streaming, a tool-use loop, batches, files and models",
    // No runtime dependencies: the JDK HttpClient and an internal JSON AST.
    // The User-Agent reads the version from here, so it never drifts from the jar.
    buildInfoKeys    := Seq[BuildInfoKey](name, version),
    buildInfoPackage := "anthropic",
    buildInfoObject  := "BuildInfo",
    buildInfoOptions += BuildInfoOption.PackagePrivate
  )

lazy val catsEffect = project
  .in(file("cats-effect"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name        := "anthropic-api-scala-sdk-cats-effect",
    description := "Cats Effect and fs2 binding for the Anthropic Messages API client",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"       % catsEffectVersion,
      "co.fs2"        %% "fs2-core"          % fs2Version,
      "org.typelevel" %% "munit-cats-effect" % munitCatsEffectVersion % Test
    )
  )

// ---- examples ---------------------------------------------------------------------------------
// Runnable samples, one `main` each. Not published, but aggregated: `sbt +test` compiles them, so a
// change to the API they document cannot be merged while they still describe the old one.
lazy val examples = project
  .in(file("examples"))
  .dependsOn(core, catsEffect)
  .settings(commonSettings)
  .settings(
    name           := "anthropic-api-scala-sdk-examples",
    publish / skip := true,
    run / fork     := true,
    run / connectInput := true,
    run / javaOptions ++= Seq("-Dfile.encoding=UTF-8", "-Dstdout.encoding=UTF-8", "-Dstderr.encoding=UTF-8")
  )
