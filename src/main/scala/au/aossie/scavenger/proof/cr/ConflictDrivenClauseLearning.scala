package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.structure.immutable.SeqClause

/**
  * @author Daniyar Itegulov
  */
case class ConflictDrivenClauseLearning(conflict: Conflict) extends CRProofNode {

  val conflictDrivenClause = conflict.findDecisions(Substitution.empty)

  override def conclusion: SeqClause = conflictDrivenClause.toSeqSequent

  override def premises: Seq[CRProofNode] = Seq(conflict)
}
