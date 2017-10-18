import com.typesafe.sbt.pgp.PgpKeys
import sbt.Keys._
import sbt._
import sbtassembly.AssemblyKeys._

object ScavengerBuild {
  lazy val commonSettings = Seq(
    organization := "au.aossie",
    version := "0.2",
    scalaVersion := "2.12.3",
    scalacOptions := Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Xfuture",
      "-Xexperimental"
    ),
    scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits"),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    test in assembly := {}
  )

  lazy val commonDeps = Seq(
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
      "org.specs2" %% "specs2-core" % "3.8.6" % Test
    )
  )

  lazy val publishSettings = Seq(
    PgpKeys.useGpg := true,
    homepage := Some(url("https://www.gitlab.com/aossie/Scavenger")),
    licenses := Seq(
      "CC BY-NC-SA" -> url(
        "http://creativecommons.org/licenses/by-nc-sa/4.0/")),
    publishArtifact in Test := false,
    publishMavenStyle := true,
    scmInfo := Some(
      ScmInfo(
        url("https://www.gitlab.com/aossie/Scavenger"),
        "git@gitlab.com:aossie/Scavenger.git"
      )
    ),
    developers ++=
      List(
        Developer(
          id = "bruno-wp",
          name = "Bruno Woltzenlogel Paleo",
          email = "bruno.wp@gmail.com",
          url = url("https://github.com/Ceilican")
        ),
        Developer(
          id = "itegulov",
          name = "Daniyar Itegulov",
          email = "ditegulov@gmail.com",
          url = url("https://github.com/itegulov")
        ),
        Developer(
          id = "vlad10795",
          name = "Padtsiolkin Uladzislau",
          email = "vlad10795@gmail.com",
          url = url("https://gitlab.com/vlad10795")
        ),
        Developer(
          id = "EzequielPostan",
          name = "Ezequiel Postan",
          email = "ezequiel_postan@hotmail.com",
          url = url("https://github.com/EzequielPostan")
        )
      ),
    pomIncludeRepository := Function.const(false),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  )

  lazy val core = Project(id = "core", base = file("core"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "scavenger-core")

  lazy val prover = Project(id = "prover", base = file("prover"))
    .settings(commonSettings ++ commonDeps)
    .settings(
      name := "scavenger-prover",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "ammonite-ops" % "1.0.2",
        "com.typesafe.akka" %% "akka-actor" % "2.5.6",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
      )
    )
    .dependsOn(core)

  lazy val commandLine = Project(id = "cli", base = file("cli"))
    .settings(commonSettings ++ commonDeps)
    .settings(
      name := "scavenger-cli",
      fullRunInputTask(scavenger, Runtime, "au.aossie.scavenger.CLI"),
      trapExit in scavenger := true,
      fork in scavenger := false,
      traceLevel in scavenger := 0,
      libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "3.7.0"
      )
    )
    .dependsOn(core, prover)

  lazy val root = Project(id = "scavenger", base = file("."))
    .aggregate(core, prover, commandLine)
    .dependsOn(core, prover, commandLine)
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(
      name := "Scavenger",
      mainClass in assembly := Some("au.aossie.scavenger.CLI"),
      assemblyJarName in assembly := "scavenger.jar",
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2-core" % "3.8.6" % "integration,end-to-end,bench",
        "com.storm-enroute" %% "scalameter" % "0.8.2"
      )
    )
    .configs(Testing.configs: _*)
    .settings(Testing.settings: _*)

  val scavenger = InputKey[Unit]("scavenger", "The Scavenger Theorem Prover")
}
