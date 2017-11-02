package org.aossie.scavenger.prover

import org.aossie.scavenger.structure.immutable.CNF

import scala.concurrent.duration.Duration

/**
  * @author Daniyar Itegulov
  */
trait Prover {
  def prove(cnf: CNF, timeout: Duration = Duration.Inf): ProblemStatus
}

