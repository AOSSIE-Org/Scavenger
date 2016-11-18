import sbt._
import Keys._

object Configs {
	val IntegrationTest = config("integration") extend(Runtime)
	val EndToEndTest = config("end-to-end") extend(Runtime)
	val PerformanceTest = config("bench") extend(Runtime)
	val all = Seq(IntegrationTest, EndToEndTest, PerformanceTest)
}