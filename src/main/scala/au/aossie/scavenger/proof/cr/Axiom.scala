package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.Clause

class Axiom(override val conclusion: Clause) extends CRProofNode {
  def auxFormulasMap = Map()
  def premises       = Seq()

  override def hashCode(): Int = super.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case ref: AnyRef => this eq ref
    case _ => false
  }
}

object Axiom {
  def apply(conclusion: Clause) = new Axiom(conclusion)
  def unapply(p: CRProofNode) = p match {
    case p: Axiom => Some(p.conclusion)
    case _        => None
  }
}
