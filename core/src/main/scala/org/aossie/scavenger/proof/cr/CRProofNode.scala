package org.aossie.scavenger.proof
package cr

import org.aossie.scavenger.structure.immutable.{Clause, Literal}


abstract class CRProofNode(val isAxiom: Boolean,
                           val decisions: Set[Literal],
                           val nonExpertDecisions: Set[Literal],
                           val decisionsWithoutSubst: Set[Literal])
  extends ProofNode[Clause, CRProofNode] {

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
