package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{ Literal, Clause }

/**
  * @author Daniyar Itegulov
  */
case class Decision(literal: Literal) extends CRProofNode {

  override def conclusion: Clause = literal.toClause
  override def premises: Seq[CRProofNode] = Seq.empty

  override def hashCode(): Int = super.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case ref: AnyRef => this eq ref
    case _ => false
  }
}

