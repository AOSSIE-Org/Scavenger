package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.structure.immutable.Clause

/**
  * @author Daniyar Itegulov
  */
class ConflictDrivenClauseLearning(val conflict: Conflict) extends CRProofNode(conflict.isAxiom) {
  val conflictDrivenClause = conflict.findDecisions(Substitution.empty)
  override def conclusion: Clause = conflictDrivenClause
  override def premises: Seq[CRProofNode] = Seq(conflict)
}

object ConflictDrivenClauseLearning {
  def apply(conflict: Conflict) =
    new ConflictDrivenClauseLearning(conflict)

  def unapply(p: CRProofNode) = p match {
    case p: ConflictDrivenClauseLearning => Some(p.conflict)
    case _ => None
  }
}
