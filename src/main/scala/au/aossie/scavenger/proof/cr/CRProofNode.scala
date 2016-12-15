package au.aossie.scavenger.proof
package cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.structure.immutable.{ SeqClause, Literal }

abstract class CRProofNode extends ProofNode[SeqClause, CRProofNode] {
  def findDecisions(sub: Substitution): SeqClause = {
    this match {
      case Decision(literal) =>
        !sub(literal)
      case conflict @ Conflict(left, right) =>
        left.findDecisions(conflict.leftMgu) union right.findDecisions(conflict.rightMgu)
      case resolution @ UnitPropagationResolution(left, right, _) =>
        // We don't need to traverse right premise, because it's either initial clause or conflict driven clause
        left
          .zip(resolution.leftMgus)
          .map {
            case (node, mgu) => node.findDecisions(mgu(sub))
          }
          .fold(SeqClause.empty)(_ union _)
      case _ =>
        SeqClause.empty
    }
  }

  def listDecisions(): Seq[Literal] = {
    this match {
      case Decision(literal) =>
        Seq(literal)
      case Conflict(left, right) =>
        left.listDecisions() ++ right.listDecisions()
      case resolution @ UnitPropagationResolution(left, _, _) =>
        left.zip(resolution.leftMgus).map(_._1.listDecisions()).fold(Seq.empty)(_ ++ _)
      case _ =>
        Seq.empty
    }
  }
}
