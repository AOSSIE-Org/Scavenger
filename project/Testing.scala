import sbt._
import Keys._

object Testing {

  val IntegrationTest = config("integration") extend Test
  val EndToEndTest = config("end-to-end") extend Test
  val PerformanceTest = config("bench") extend Test
  val AdHocTest = config("adhoc") extend Test

  val configs = Seq(IntegrationTest, EndToEndTest, PerformanceTest, AdHocTest)

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

  private lazy val adhocTestSettings =
    inConfig(AdHocTest)(Defaults.testSettings) ++
    Seq(
      fork in AdHocTest := false,
      parallelExecution in AdHocTest := false,
      scalaSource in AdHocTest := baseDirectory.value / "src/adhoc/scala")

  lazy val testAllQuick = TaskKey[Unit]("test-all-quick")

  lazy val testAllQuickSettings = Seq(
    testAll := (),
    testAll := testAll.dependsOn(test in EndToEndTest),
    testAll := testAll.dependsOn(test in IntegrationTest),
    testAll := testAll.dependsOn(test in Test)
  )

  lazy val testAll = TaskKey[Unit]("test-all")

  lazy val testAllSettings = Seq(
    testAll := (),
    testAll := testAll.dependsOn(test in EndToEndTest),
    testAll := testAll.dependsOn(test in IntegrationTest),
    testAll := testAll.dependsOn(test in PerformanceTest),
    testAll := testAll.dependsOn(test in Test)
  )

  lazy val settings = integrationTestSettings ++
                      endToEndTestSettings ++
                      performanceTestSettings ++
                      adhocTestSettings ++
                      testAllQuickSettings ++
                      testAllSettings
}
