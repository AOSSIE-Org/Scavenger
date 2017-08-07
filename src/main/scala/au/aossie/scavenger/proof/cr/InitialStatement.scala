package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.structure.immutable.{AxiomClause, Clause, Literal}

import scala.collection.mutable

class InitialStatement(override val conclusion: Clause, override val nonExpertDecisions: mutable.Set[Literal] = mutable.Set.empty)
  extends CRProofNode(
    conclusion.tp == AxiomClause,
    mutable.Set.empty,
    nonExpertDecisions
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
