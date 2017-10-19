package org.aossie.scavenger.prover.choosing

import org.aossie.scavenger.structure.immutable.Literal

import scala.util.Random

/**
  * Created by podtelkin on 16.08.17.
  */
class DecisionsMaker(maxIterationsWithoutDecision: Int)(implicit rnd: Random) {
  var counter: Int = 0

  def incCounter: Unit = counter += 1

  def counterExpired: Boolean = counter >= maxIterationsWithoutDecision

  def makeDecision(available: Seq[Literal]): Literal = {
    counter = 0
    available(rnd.nextInt(available.size))
  }
}
