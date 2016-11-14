package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{Clause => Sequent}



class Axiom(override val conclusion: Sequent) extends SequentProofNode {
  def auxFormulasMap = Map()
  def premises = Seq()
}

object Axiom {
  def apply(conclusion: Sequent) = new Axiom(conclusion)
  def unapply(p: SequentProofNode) = p match {
    case p: Axiom => Some(p.conclusion)
    case _ => None
  }
}