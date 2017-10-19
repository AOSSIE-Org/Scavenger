package org.aossie.scavenger.prover

import org.aossie.scavenger.expression.Sym
import org.aossie.scavenger.proof.cr.{CRProof => Proof}

import org.aossie.scavenger.structure.immutable.CNF

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
trait Prover {
  def prove(cnf: CNF): ProblemStatus
}

