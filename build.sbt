name := "Scavenger"

organization := "au.aossie"

version := "0.2"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize", "-Xdisable-assertions")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases", "public").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.6" % "test,integration,end-to-end,bench",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
//  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.4",
  "com.typesafe.akka" %% "akka-actor" % "2.4.12",
  "com.storm-enroute" %% "scalameter" % "0.8.2",
  "com.lihaoyi" %% "ammonite-ops" % "0.8.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.6.0"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
  
logBuffered := false


licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://www.gitlab.com/aossie/Scavenger"))

val scavenger = InputKey[Unit]("scavenger", "The Scavenger Theorem Prover")

lazy val project = Project(id = "scavenger", base = file("."))
  .configs(Testing.configs: _*)
  .settings(Testing.settings: _*)
  .settings(
    mainClass in assembly := Some("au.aossie.scavenger.CLI"),
    fullRunInputTask(scavenger, Runtime, "au.aossie.scavenger.CLI"),
    trapExit in scavenger := true,
    fork in scavenger := false,
    traceLevel in scavenger := 0
  )
