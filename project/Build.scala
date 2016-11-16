import sbt._
import Keys._
import sys.process._
import com.github.retronym.SbtOneJar._

import language.postfixOps


object ScavengerBuild extends Build {

  private def major(version: String) = version.split("[.]").take(2).mkString(".")

  // Extension of "one-jar" to copy jar file to the root folder
  val jarS = oneJar <<= (oneJar,scalaVersion,version) map { (file,s,v) =>
    val m = major(s)
    sys.process.stringToProcess("cp ./target/scala-" + m + "/scavenger_" + m + "-" + v + "-one-jar.jar scavenger.jar") !;
    file
  }
  val jarSettings = oneJarSettings ++
                    Seq(jarS,
                        mainClass in oneJar := Some("au.aossie.scavenger.CLI"))


  // Custom Run Tasks
  val scavenger = InputKey[Unit]("scavenger", "The Scavenger Theorem Prover")
  val scavengerSettings = Seq(fullRunInputTask(scavenger,Runtime,"au.aossie.scavenger.CLI"),
                              trapExit in scavenger := true ,
                              fork in scavenger := false,
                              traceLevel in scavenger := 0)


  val allSettings = Defaults.coreDefaultSettings ++
                    jarSettings ++
                    scavengerSettings


  lazy val project = Project("project", file("."), settings = allSettings)
}