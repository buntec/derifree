Global / onChangedBuildSource := ReloadOnSourceChanges
Global / resolvers += "Sonatype S01 OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots"

ThisBuild / tlBaseVersion := "0.0"

lazy val scala3 = "3.3.1"
ThisBuild / scalaVersion := scala3
ThisBuild / crossScalaVersions := Seq(scala3)
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / startYear := Some(2023)
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / developers := List(
  tlGitHubDev("buntec", "Christoph Bunte")
)

ThisBuild / tlFatalWarnings := false

// we need at least java 11 for ejml
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
ThisBuild / githubWorkflowPublishTargetBranches := Seq.empty

lazy val V = new {
  val cats = "2.10.0"
  val catsEffect = "3.5.3"
  val fs2 = "3.9.4"
  val catsTime = "0.5.1"
  val kittens = "3.2.0"
  val circe = "0.14.6"
  val literally = "1.1.0"
  val commonsMath = "3.6.1"
  val ejml = "0.43.1"
  val munit = "0.7.29"
  val munitCE = "2.0.0-M4"
  val parallelCollection = "1.0.4"
}

lazy val root = tlCrossRootProject.aggregate(derifree, examples)

lazy val derifree = (project in file("derifree"))
  .settings(
    name := "derifree",
    fork := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % V.cats,
      "org.typelevel" %% "cats-free" % V.cats,
      "org.typelevel" %% "alleycats-core" % V.cats,
      "org.typelevel" %% "kittens" % V.kittens,
      "org.typelevel" %% "cats-time" % V.catsTime,
      "org.typelevel" %% "literally" % V.literally,
      "io.circe" %% "circe-core" % V.circe,
      "io.circe" %% "circe-generic" % V.circe,
      "io.circe" %% "circe-literal" % V.circe,
      "io.circe" %% "circe-parser" % V.circe,
      "org.apache.commons" % "commons-math3" % V.commonsMath,
      "org.ejml" % "ejml-all" % V.ejml,
      "org.scalameta" %% "munit" % V.munit % Test,
      "org.typelevel" %%% "munit-cats-effect" % V.munitCE % Test,
      "org.scala-lang.modules" %% "scala-parallel-collections" % V.parallelCollection % Test,
      "org.typelevel" %% "cats-effect" % V.catsEffect % Test,
      "org.typelevel" %% "cats-effect-kernel" % V.catsEffect % Test,
      "org.typelevel" %% "cats-effect-std" % V.catsEffect % Test,
      "co.fs2" %% "fs2-core" % V.fs2 % Test,
      "co.fs2" %% "fs2-io" % V.fs2 % Test
    )
  )

lazy val docs = (project in file("derifree-docs"))
  .enablePlugins(MdocPlugin)
  .settings(
    mdocOut := file("."),
    mdocVariables := Map(
      "VERSION" -> version.value
    )
  )
  .dependsOn(derifree)

lazy val examples = (project in file("examples"))
  .enablePlugins(NoPublishPlugin)
  .settings(fork := true)
  .dependsOn(derifree)
