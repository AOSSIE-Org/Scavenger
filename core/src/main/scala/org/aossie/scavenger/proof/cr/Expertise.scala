package org.aossie.scavenger.proof.cr

import org.aossie.scavenger.expression.{AppRec, Sym}
import org.aossie.scavenger.structure.immutable.{Clause, Literal}

/**
  * Created by podtelkin on 16.08.17.
  */
class Expertise(val node: CRProofNode,
                val predicates: Set[Sym])(private val expertLiterals: Seq[Literal],
                                          private val nonExpertLiterals: Seq[Literal])
  extends CRProofNode(
    node.isAxiom,
    Set.empty,
    nonExpertLiterals.toSet,
    Set.empty
  ) {

  override def conclusion: Clause = expertLiterals.toClause
  override def premises: Seq[CRProofNode] = Seq(node)
}

object Expertise {

  def apply(node: CRProofNode, predicates: Set[Sym]) = {
    val (expertLiterals, nonExpertLiterals) = node.conclusion.literals.partition(literal => literal.unit match {
      case AppRec(p: Sym, _) =>
        predicates.contains(p)
    } )
    new Expertise(node, predicates)(expertLiterals, nonExpertLiterals)
  }

  def unapply(p: CRProofNode) = p match {
    case p: Expertise =>
      Some((p.conclusion, p.predicates))
    case _ =>
      None
  }
}