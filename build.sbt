name := "Scavenger"

organization := "au.aossie"

version := "0.1"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases", "public").map(Resolver.sonatypeRepo)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.specs2" %% "specs2" % "2.4.15" % "test",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "org.scala-lang" % "scala-library" % "2.11.6", // apparently needed because of timeout and deprecated actors library
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1", // seems inactive
  "com.typesafe.akka" %% "akka-actor" % "2.4.9"
)


licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://gitlab.com/AOSSIE/Scav"))
