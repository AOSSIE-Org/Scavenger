package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.structure.immutable.Clause

/**
  * @author Daniyar Itegulov
  */
case class ConflictDrivenClauseLearning(conflict: Conflict) extends SequentProofNode {

  private def findDecisions(proofNode: SequentProofNode, sub: Substitution): Clause = {
    proofNode match {
      case Decision(literal) =>
        !sub(literal)
      case conflict@Conflict(left, right) =>
        findDecisions(left, conflict.leftMgu) union findDecisions(right, conflict.rightMgu)
      case resolution@UnitPropagationResolution(left, right, _) =>
        // We don't need to traverse right premise, because it's either initial clause or conflict driven clause
        left.zip(resolution.leftMgus).map {
          case (node, mgu) => findDecisions(node, mgu(sub))
        }.fold(Clause()())(_ union _)
      case _ =>
        Clause()()
    }
  }

  val conflictDrivenClause = unique(findDecisions(conflict, Substitution.empty))

  override def conclusion: Clause = conflictDrivenClause.toSeqSequent

  override def premises: Seq[SequentProofNode] = Seq(conflict)
}
