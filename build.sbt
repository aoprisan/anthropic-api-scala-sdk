val scala213 = "2.13.18"
val scala3   = "3.3.6"
// Ox and Kyo are published for this Scala only; their modules are built with it alone.
val scala39 = "3.9.0"

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

// Every module targets this JDK, except the two whose library needs a later one: Ox (21) and Kyo (25).
val baseRelease = "17"
val oxRelease   = "21"
val kyoRelease  = "25"

// A module is part of the build only on a JDK that can compile it, so `sbt +test` works on any JDK from
// 17 on and covers what that JDK allows. Releases are cut on a JDK that has them all.
val jdk     = java.lang.Runtime.version().feature()
val hasOx   = jdk >= oxRelease.toInt
val hasKyo  = jdk >= kyoRelease.toInt

val munitVersion           = "1.1.1"
val catsEffectVersion      = "3.7.1"
val fs2Version             = "3.14.0"
val munitCatsEffectVersion = "2.2.1"
val monixVersion           = "3.5.2"
val zioVersion             = "2.1.27"
val oxVersion              = "1.1.0"
val kyoVersion             = "1.0.0-RC7"

/** One source tree for both Scala versions: Scala 2.13 reads it with `-Xsource:3`. */
def commonSettings: Seq[Setting[_]] = commonSettings(baseRelease)

def commonSettings(release: String): Seq[Setting[_]] = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-release", release) ++ {
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

/** A module for Scala 3.9 only, targeting the given JDK. */
def scala39Settings(release: String): Seq[Setting[_]] = commonSettings(release) ++ Seq(
  scalaVersion       := scala39,
  crossScalaVersions := Seq(scala39)
)

lazy val root = project
  .in(file("."))
  .aggregate(core, catsEffect, monix, zio, examples, examplesMonix)
  .aggregate((if (hasOx) Seq[ProjectReference](ox, examplesOx) else Nil): _*)
  .aggregate((if (hasKyo) Seq[ProjectReference](kyo, examplesKyo) else Nil): _*)
  .settings(
    name           := "anthropic-api-scala-sdk-root",
    publish / skip := true,
    // The modules do not all share their Scala versions, so `+` must go through each module with its
    // own list instead of switching the root and running everything under it.
    crossScalaVersions := Nil
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

// Monix 3 runs on Cats Effect 2, so this module (and anything using it) cannot share a classpath with
// the Cats Effect 3 one.
lazy val monix = project
  .in(file("monix"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name        := "anthropic-api-scala-sdk-monix",
    description := "Monix binding (Task and Observable) for the Anthropic Messages API client",
    libraryDependencies += "io.monix" %% "monix" % monixVersion
  )

lazy val zio = project
  .in(file("zio"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name        := "anthropic-api-scala-sdk-zio",
    description := "ZIO 2 binding (ZIO, ZStream and ZLayer) for the Anthropic Messages API client",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"         % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion
    )
  )

// Scala 3.9 and JDK 21: Ox runs on virtual threads and structured concurrency.
lazy val ox = project
  .in(file("ox"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(scala39Settings(oxRelease))
  .settings(
    name        := "anthropic-api-scala-sdk-ox",
    description := "Ox binding (Flow and structured concurrency) for the Anthropic Messages API client",
    libraryDependencies += "com.softwaremill.ox" %% "core" % oxVersion
  )

// Scala 3.9 and JDK 25, as Kyo itself.
lazy val kyo = project
  .in(file("kyo"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(kyoSettings)
  .settings(
    name        := "anthropic-api-scala-sdk-kyo",
    description := "Kyo binding (Async, Abort and Stream) for the Anthropic Messages API client",
    libraryDependencies += "io.getkyo" %% "kyo-core" % kyoVersion
  )

// ---- examples ---------------------------------------------------------------------------------
// Runnable samples, one `main` each. Not published, but aggregated: `sbt +test` compiles them, so a
// change to the API they document cannot be merged while they still describe the old one.
lazy val examples = project
  .in(file("examples"))
  .dependsOn(core, catsEffect, zio)
  .settings(commonSettings)
  .settings(
    name           := "anthropic-api-scala-sdk-examples",
    publish / skip := true,
    run / fork     := true,
    run / connectInput := true,
    run / javaOptions ++= Seq("-Dfile.encoding=UTF-8", "-Dstdout.encoding=UTF-8", "-Dstderr.encoding=UTF-8")
  )

// The Monix sample lives apart: Monix brings Cats Effect 2, the other examples Cats Effect 3.
lazy val examplesMonix = project
  .in(file("examples-monix"))
  .dependsOn(core, monix)
  .settings(commonSettings)
  .settings(
    name           := "anthropic-api-scala-sdk-examples-monix",
    publish / skip := true,
    run / fork     := true,
    run / javaOptions ++= Seq("-Dfile.encoding=UTF-8", "-Dstdout.encoding=UTF-8", "-Dstderr.encoding=UTF-8")
  )

// Kyo's inline methods carry `@nowarn`s that have nothing to silence once inlined here, which
// `-Wunused:all` reports.
def kyoSettings: Seq[Setting[_]] = scala39Settings(kyoRelease) ++ Seq(
  scalacOptions += "-Wconf:msg=@nowarn annotation does not suppress any warnings:s"
)

def exampleSettings: Seq[Setting[_]] = Seq(
  publish / skip := true,
  run / fork     := true,
  run / javaOptions ++= Seq("-Dfile.encoding=UTF-8", "-Dstdout.encoding=UTF-8", "-Dstderr.encoding=UTF-8")
)

lazy val examplesOx = project
  .in(file("examples-ox"))
  .dependsOn(core, ox)
  .settings(scala39Settings(oxRelease))
  .settings(exampleSettings)
  .settings(name := "anthropic-api-scala-sdk-examples-ox")

lazy val examplesKyo = project
  .in(file("examples-kyo"))
  .dependsOn(core, kyo)
  .settings(kyoSettings)
  .settings(exampleSettings)
  .settings(name := "anthropic-api-scala-sdk-examples-kyo")
