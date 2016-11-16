package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.proof.cr.{CRProof => Proof}

import au.aossie.scavenger.structure.immutable.CNF

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
trait Prover {
  def prove(cnf: CNF)(implicit variables: mutable.Set[Sym]): ProblemStatus
}

