package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{ Literal, Clause }

/**
  * @author Daniyar Itegulov
  */
class Decision(val literal: Literal) extends CRProofNode {

  override def conclusion: Clause = literal.toClause
  override def premises: Seq[CRProofNode] = Seq.empty
}

object Decision {
  def apply(literal: Literal) = new Decision(literal)
  def unapply(p: CRProofNode) = p match {
    case p: Decision => Some(p.literal)
    case _ => None
  }
}

