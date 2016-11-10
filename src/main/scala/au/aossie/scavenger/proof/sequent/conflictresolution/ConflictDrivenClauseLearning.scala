package au.aossie.scavenger.proof.sequent.conflictresolution

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.judgment.immutable.SeqSequent
import au.aossie.scavenger.proof.sequent.SequentProofNode

/**
  * @author Daniyar Itegulov
  */
case class ConflictDrivenClauseLearning(conflict: Conflict) extends SequentProofNode {

  private def findDecisions(proofNode: SequentProofNode, sub: Substitution): SeqSequent = {
    proofNode match {
      case Decision(literal) =>
        !sub(literal)
      case conflict@Conflict(left, right) =>
        findDecisions(left, conflict.leftMgu) union findDecisions(right, conflict.rightMgu)
      case resolution@UnitPropagationResolution(left, right, _) =>
        // We don't need to traverse right premise, because it's either initial clause or conflict driven clause
        left.zip(resolution.leftMgus).map {
          case (node, mgu) => findDecisions(node, mgu(sub))
        }.fold(SeqSequent()())(_ union _)
      case _ =>
        SeqSequent()()
    }
  }

  val conflictDrivenClause = unique(findDecisions(conflict, Substitution.empty))

  override def auxFormulasMap: Map[SequentProofNode, SeqSequent] = Map.empty

  override def mainFormulas: SeqSequent = SeqSequent()()

  override def conclusionContext: SeqSequent = conflictDrivenClause.toSeqSequent

  override def premises: Seq[SequentProofNode] = Seq(conflict)
}
