package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.Var
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.sequent.SequentProofNode

import au.aossie.scavenger.structure.immutable.CNF

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
trait Prover {
  def prove(cnf: CNF)(implicit variables: mutable.Set[Var]): Option[Proof[SequentProofNode]]
}
