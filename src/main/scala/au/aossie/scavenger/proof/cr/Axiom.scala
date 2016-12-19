package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{ SetClause => Clause }

class Axiom(override val conclusion: Clause) extends CRProofNode {
  def auxFormulasMap = Map()
  def premises       = Seq()
}

object Axiom {
  def apply(conclusion: Clause) = new Axiom(conclusion)
  def unapply(p: CRProofNode) = p match {
    case p: Axiom => Some(p.conclusion)
    case _        => None
  }
}
