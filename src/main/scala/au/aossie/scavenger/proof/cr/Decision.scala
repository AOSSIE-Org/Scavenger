package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.Literal
import au.aossie.scavenger.structure.immutable.Clause

/**
  * @author Daniyar Itegulov
  */
case class Decision(literal: Literal) extends SequentProofNode {

  override def conclusion: Clause = literal.toClause

  override def premises: Seq[SequentProofNode] = Seq.empty
}
