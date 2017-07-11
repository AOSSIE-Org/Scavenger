package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.structure.immutable.Clause

import scala.collection.mutable

/**
  * Represents Conflict rule from Conflict Resolution calculus.
  *
  * @author Daniyar Itegulov
  */
class Conflict(val leftPremise: CRProofNode, val rightPremise: CRProofNode)
  extends CRProofNode(leftPremise.isAxiom & rightPremise.isAxiom) {
  assert(leftPremise.conclusion.isUnit, "Left premise should be a unit clause")
  assert(rightPremise.conclusion.isUnit, "Right premise should be a unit clause")
  assert(leftPremise.conclusion.literal.polarity != rightPremise.conclusion.literal.polarity, "Left and right premises should have different polarity")

  val leftAux = leftPremise.conclusion.literals.head.unit
  val rightAux = rightPremise.conclusion.literals.head.unit

  val (Seq(leftMgu), rightMgu) = unifyWithRename(Seq(leftAux), Seq(rightAux)) match {
    case None => throw new Exception("Conflict: given premise clauses are not resolvable")
    case Some(u) => u
  }

  override def premises = Seq(leftPremise, rightPremise)
  override def conclusion: Clause = Clause.empty
}

object Conflict {
  def apply(leftPremise: CRProofNode, rightPremise: CRProofNode) =
    new Conflict(leftPremise: CRProofNode, rightPremise: CRProofNode)

  def unapply(p: Conflict): Option[(CRProofNode, CRProofNode)] = p match {
    case p: Conflict => Some((p.leftPremise, p.rightPremise))
    case _ => None
  }
}

