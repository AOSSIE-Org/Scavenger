package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.structure.immutable.{ SetClause => Clause }

import scala.collection.mutable

/**
  * Represents Conflict rule from Conflict Resolution calculus.
  *
  * @author Daniyar Itegulov
  */
case class Conflict(leftPremise: CRProofNode, rightPremise: CRProofNode)
  extends CRProofNode {
  require(leftPremise.conclusion.width == 1, "Left premise should be a unit clause")
  require(rightPremise.conclusion.width == 1, "Right premise should be a unit clause")

  private val leftAux = leftPremise.conclusion.literals.head.unit
  private val rightAux = rightPremise.conclusion.literals.head.unit

  val (Seq(leftMgu), rightMgu) = unifyWithRename(Seq(leftAux), Seq(rightAux)) match {
    case None => throw new Exception("Conflict: given premise clauses are not resolvable")
    case Some(u) => u
  }

  override def premises = Seq(leftPremise, rightPremise)
  override def conclusion: Clause = Clause.empty
}

