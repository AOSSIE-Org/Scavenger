package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.Clause
import au.aossie.scavenger.prover._

/**
  * @author Daniyar Itegulov
  */
class ConflictDrivenClauseLearning(val conflict: Conflict)
  extends CRProofNode(
    conflict.isAxiom,
    Set.empty,
    conflict.nonExpertDecisions,
    Set.empty
  ) {
  lazy val conflictDrivenClause: Clause = (conflict.decisions.map(!_) ++ conflict.nonExpertDecisions).toClause
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
