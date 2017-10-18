package au.aossie.scavenger.util

import scala.concurrent.duration._
import scala.language.postfixOps

package object pretty {
  case class Timed[+R](result: R, time: FiniteDuration)

  def timed[R](f: => R): Timed[R] = {
    System.gc()
    val start = System.nanoTime
    val result = f
    val end = System.nanoTime
    val time: FiniteDuration = (end - start).nanos
    Timed(result, time)
  }

  def timed[R](repetitions: Int)(f: => R): Timed[R] = {
    val averageTime = (1 to repetitions).map(_ => timed(f).time).foldLeft(0 seconds)(_ + _) / repetitions
    Timed(f, averageTime)
  }
}

