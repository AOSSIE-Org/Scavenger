package au.aossie.scavenger.proof.sequent.conflictresolution

import au.aossie.scavenger.algorithm.prover._
import au.aossie.scavenger.expression.Var
import au.aossie.scavenger.judgment.immutable.SeqSequent
import au.aossie.scavenger.proof.sequent.{Binary, SequentProofNode}

import scala.collection.mutable

/**
  * Represents Conflict rule from Conflict Resolution calculus.
  *
  * @author Daniyar Itegulov
  */
case class Conflict(leftPremise: SequentProofNode, rightPremise: SequentProofNode)(implicit variables: mutable.Set[Var])
  extends SequentProofNode with Binary {
  require(leftPremise.conclusion.width == 1, "Left premise should be a unit clause")
  require(rightPremise.conclusion.width == 1, "Right premise should be a unit clause")

  private val leftAux = leftPremise.conclusion.literals.head.unit
  private val rightAux = rightPremise.conclusion.literals.head.unit

  val (Seq(leftMgu), rightMgu) = unifyWithRename(Seq(leftAux), Seq(rightAux)) match {
    case None => throw new Exception("Conflict: given premise clauses are not resolvable")
    case Some(u) => u
  }

  override def mainFormulas: SeqSequent = SeqSequent()()

  override def conclusionContext: SeqSequent = SeqSequent()()

  override def leftAuxFormulas: SeqSequent = leftPremise.conclusion

  override def rightAuxFormulas: SeqSequent = rightPremise.conclusion
}
