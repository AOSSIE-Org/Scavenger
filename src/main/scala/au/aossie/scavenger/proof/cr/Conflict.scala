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
case class Conflict(leftPremise: CRProofNode, rightPremise: CRProofNode)
  extends CRProofNode {
  require(leftPremise.conclusion.isUnit, "Left premise should be a unit clause")
  require(rightPremise.conclusion.isUnit, "Right premise should be a unit clause")
  require(leftPremise.conclusion.literal.negated != rightPremise.conclusion.literal.negated, "Left and right premises should have different negation")

  private val leftAux = leftPremise.conclusion.literals.head.unit
  private val rightAux = rightPremise.conclusion.literals.head.unit

  val (Seq(leftMgu), rightMgu) = unifyWithRename(Seq(leftAux), Seq(rightAux)) match {
    case None => throw new Exception("Conflict: given premise clauses are not resolvable")
    case Some(u) => u
  }

  override def premises = Seq(leftPremise, rightPremise)
  override def conclusion: Clause = Clause.empty

  override def hashCode(): Int = super.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case ref: AnyRef => this eq ref
    case _ => false
  }
}

