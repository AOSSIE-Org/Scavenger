package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.unification.tools._
import au.aossie.scavenger.structure.immutable.Clause

/**
  * Represents Conflict rule from Conflict Resolution calculus.
  *
  * @author Daniyar Itegulov
  */
class Conflict(val leftPremise: CRProofNode, val rightPremise: CRProofNode)(private val leftMgu: Substitution,
                                                                            private val rightMgu: Substitution)
  extends CRProofNode(
    leftPremise.isAxiom & rightPremise.isAxiom,
    leftPremise.decisions.map(literal => leftMgu(literal)) ++
      rightPremise.decisions.map(literal => rightMgu(literal)),
    leftPremise.nonExpertDecisions.map(literal => leftMgu(literal)) ++
      rightPremise.nonExpertDecisions.map(literal => rightMgu(literal)),
    leftPremise.decisionsWithoutSubst ++ rightPremise.decisionsWithoutSubst
  ) {
  assert(leftPremise.conclusion.isUnit, "Left premise should be a unit clause")
  assert(rightPremise.conclusion.isUnit, "Right premise should be a unit clause")
  assert(leftPremise.conclusion.literal.polarity != rightPremise.conclusion.literal.polarity, "Left and right premises should have different polarity")

  val leftAux = leftPremise.conclusion.literals.head.unit
  val rightAux = rightPremise.conclusion.literals.head.unit

  override def premises = Seq(leftPremise, rightPremise)
  override def conclusion: Clause = Clause.empty
}

object Conflict {
  def apply(leftPremise: CRProofNode, rightPremise: CRProofNode) = {
    assert(leftPremise.conclusion.isUnit, "Left premise should be a unit clause")
    assert(rightPremise.conclusion.isUnit, "Right premise should be a unit clause")
    assert(leftPremise.conclusion.literal.polarity != rightPremise.conclusion.literal.polarity, "Left and right premises should have different polarity")

    val (Seq(leftMgu), rightMgu) = unifyWithRename(
      Seq(leftPremise.conclusion.literals.head.unit),
      Seq(rightPremise.conclusion.literals.head.unit)
    ) match {
      case None => throw new Exception("Conflict: given premise clauses are not resolvable")
      case Some(u) => u
    }

    new Conflict(leftPremise: CRProofNode, rightPremise: CRProofNode)(leftMgu, rightMgu)
  }

  def unapply(p: Conflict): Option[(CRProofNode, CRProofNode)] = p match {
    case p: Conflict => Some((p.leftPremise, p.rightPremise))
    case _ => None
  }
}

