package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.structure.immutable.SetClause

/**
  * @author Daniyar Itegulov
  */
case class ConflictDrivenClauseLearning(conflict: Conflict) extends CRProofNode {

  private def findDecisions(proofNode: CRProofNode, sub: Substitution): SetClause = {
    proofNode match {
      case Decision(literal) =>
        !sub(literal)
      case conflict@Conflict(left, right) =>
        findDecisions(left, conflict.leftMgu) union findDecisions(right, conflict.rightMgu)
      case resolution@UnitPropagationResolution(left, right, _) =>
        // We don't need to traverse right premise, because it's either initial clause or conflict driven clause
        left.zip(resolution.leftMgus).map {
          case (node, mgu) => findDecisions(node, mgu(sub))
        }.fold(SetClause()())(_ union _)
      case _ =>
        SetClause()()
    }
  }

  val conflictDrivenClause = findDecisions(conflict, Substitution.empty)

  override def conclusion: SetClause = conflictDrivenClause.toSetSequent

  override def premises: Seq[CRProofNode] = Seq(conflict)
}
