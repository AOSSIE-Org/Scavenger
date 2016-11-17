package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{ Literal, SetClause }

/**
  * @author Daniyar Itegulov
  */
case class Decision(literal: Literal) extends CRProofNode {

  override def conclusion: SetClause = literal.toClause

  override def premises: Seq[CRProofNode] = Seq.empty
}

