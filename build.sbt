name := "Scavenger"

organization := "au.aossie"

version := "0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize", "-Yinline-warnings")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases", "public").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.6" % "test,integration,end-to-end,bench",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1", // seems inactive
  "com.typesafe.akka" %% "akka-actor" % "2.4.12",
  "com.storm-enroute" %% "scalameter" % "0.7",
  "com.lihaoyi" %% "ammonite-ops" % "0.8.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.6.0"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
  
logBuffered := false


licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://www.gitlab.com/aossie/Scavenger"))
