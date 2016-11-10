package au.aossie.scavenger.proof
package sequent
package lk

import au.aossie.scavenger.structure.immutable.{Clause => Sequent}
import au.aossie.scavenger.expression.E


class Axiom(override val mainFormulas: Sequent) extends SequentProofNode
with Nullary with NoImplicitContraction

object Axiom {
  def apply(conclusion: Sequent) = new Axiom(conclusion)
  def unapply(p: SequentProofNode) = p match {
    case p: Axiom => Some(p.conclusion)
    case _ => None
  }
}