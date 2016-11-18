import sbt._
import Keys._

object Testing {
  
  import Configs._

  lazy val testAll = TaskKey[Unit]("test-all")

  private lazy val integrationTestSettings =
    inConfig(IntegrationTest)(Defaults.testSettings) ++
    Seq(
      fork in IntegrationTest := false,
      parallelExecution in IntegrationTest := false,
      scalaSource in IntegrationTest := baseDirectory.value / "src/integration/scala")

  private lazy val endToEndTestSettings =
    inConfig(EndToEndTest)(Defaults.testSettings) ++
    Seq(
      fork in EndToEndTest := false,
      parallelExecution in EndToEndTest := false,
      scalaSource in EndToEndTest := baseDirectory.value / "src/end-to-end/scala")

  private lazy val performanceTestSettings =
    inConfig(PerformanceTest)(Defaults.testSettings) ++
    Seq(
      fork in PerformanceTest := false,
      parallelExecution in PerformanceTest := false,
      scalaSource in PerformanceTest := baseDirectory.value / "src/bench/scala")

  lazy val settings = integrationTestSettings ++ 
                      endToEndTestSettings ++ 
                      performanceTestSettings ++ 
  Seq(
  	testAll := (),
  	testAll <<= testAll.dependsOn(test in EndToEndTest),
    testAll <<= testAll.dependsOn(test in IntegrationTest),
    testAll <<= testAll.dependsOn(test in PerformanceTest),
    testAll <<= testAll.dependsOn(test in Test)
  )
}
