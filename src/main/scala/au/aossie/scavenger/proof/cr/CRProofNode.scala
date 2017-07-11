package au.aossie.scavenger.proof
package cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.structure.immutable.{ Clause, Literal }

abstract class CRProofNode(val isAxiom: Boolean) extends ProofNode[Clause, CRProofNode] {

  def findDecisions(sub: Substitution): Clause = {
    this match {
      case Decision(literal) =>
        !sub(literal)
      case conflict @ Conflict(left, right) =>
        left.findDecisions(conflict.leftMgu) union right.findDecisions(conflict.rightMgu)
      case UnitPropagationResolution(left, right, _, leftMgus, _) =>
        // We don't need to traverse right premise, because it's either initial clause or conflict driven clause
        left
          .zip(leftMgus)
          .map {
            case (node, mgu) => node.findDecisions(mgu(sub))
          }
          .fold(Clause.empty)(_ union _)
      case _ =>
        Clause.empty
    }
  }

  def listDecisions(): Seq[Literal] = {
    this match {
      case Decision(literal) =>
        Seq(literal)
      case Conflict(left, right) =>
        left.listDecisions() ++ right.listDecisions()
      case UnitPropagationResolution(left, _, _, leftMgus, _) =>
        left.zip(leftMgus).map(_._1.listDecisions()).fold(Seq.empty)(_ ++ _)
      case _ =>
        Seq.empty
    }
  }
}
