package org.aossie.scavenger.proof.cr

import org.aossie.scavenger.structure.immutable.{AxiomClause, Clause}

class InitialStatement(override val conclusion: Clause)
  extends CRProofNode(
    conclusion.tp == AxiomClause,
    Set.empty,
    Set.empty,
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