package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{ Literal, SetClause => Clause }

/**
  * @author Daniyar Itegulov
  */
case class Decision(literal: Literal) extends CRProofNode {

  override def conclusion: Clause = literal.toSetClause

  override def premises: Seq[CRProofNode] = Seq.empty
}

