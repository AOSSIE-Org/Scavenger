package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{AxiomClause, Clause}

class InitialStatement(override val conclusion: Clause) extends CRProofNode(conclusion.tp == AxiomClause) {
  def auxFormulasMap = Map()
  def premises       = Seq()
}

object InitialStatement {
  def apply(conclusion: Clause) = new InitialStatement(conclusion)
  def unapply(p: CRProofNode) = p match {
    case p: InitialStatement => Some(p.conclusion)
    case _        => None
  }
}
