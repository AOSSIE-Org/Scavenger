package au.aossie.scavenger.proof.sequent.conflictresolution

import au.aossie.scavenger.algorithm.prover.structure.immutable.Literal
import au.aossie.scavenger.judgment.immutable.SeqSequent
import au.aossie.scavenger.proof.sequent.SequentProofNode

/**
  * @author Daniyar Itegulov
  */
case class Decision(literal: Literal) extends SequentProofNode {
  override def auxFormulasMap: Map[SequentProofNode, SeqSequent] = Map.empty

  override def mainFormulas: SeqSequent = SeqSequent()()

  override def conclusionContext: SeqSequent = literal.toClause

  override def premises: Seq[SequentProofNode] = Seq.empty
}
