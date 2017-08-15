package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{AxiomClause, Clause, Literal}

class InitialStatement(override val conclusion: Clause, override val nonExpertDecisions: Set[Literal] = Set.empty)
  extends CRProofNode(
    conclusion.tp == AxiomClause,
    Set.empty,
    nonExpertDecisions,
    Set.empty
  ) {
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
