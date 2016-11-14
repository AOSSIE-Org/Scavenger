package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.Literal
import au.aossie.scavenger.structure.immutable.Clause

/**
  * @author Daniyar Itegulov
  */
case class Decision(literal: Literal) extends SequentProofNode {
  override def auxFormulasMap: Map[SequentProofNode, Clause] = Map.empty

  override def mainFormulas: Clause = Clause()()

  override def conclusionContext: Clause = literal.toClause

  override def premises: Seq[SequentProofNode] = Seq.empty
}
