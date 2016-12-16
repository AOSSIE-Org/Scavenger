package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.structure.immutable.{ Literal, SeqClause }
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.expression.substitution.immutable.Substitution

import scala.collection.mutable

/**
  * Represents Unit-Propagation Resolution rule from Conflict Resolution calculus.
  *
  * @author Daniyar Itegulov
  */
case class UnitPropagationResolution private (
    left: Seq[CRProofNode],
    right: CRProofNode,
    desired: Literal,
    leftMgus: Seq[Substitution],
    rmgu: Substitution)(implicit variables: mutable.Set[Sym])
    extends CRProofNode {
  require(left.forall(_.conclusion.width == 1), "All left conclusions should be unit")
  require(left.size + 1 == right.conclusion.width,
          "There should be enough left premises to derive desired")
  val leftLiterals = left.map(_.conclusion.literals.head)

  override def conclusion: SeqClause = desired

  override def premises: Seq[CRProofNode] = left :+ right
}

object UnitPropagationResolution {
  def apply(left: Seq[CRProofNode], right: CRProofNode)(
      implicit variables: mutable.Set[Sym]): UnitPropagationResolution = {
    val leftLiterals = left.map(_.conclusion.literals.head)
    right.conclusion.literals.indices
      .map(desiredIndex => {
        // Find such desired index that remaining right literals will be unifiable with left literals
        val rightLiterals = right.conclusion.literals.patch(desiredIndex, Nil, 1)
        if (!leftLiterals.zip(rightLiterals).forall { case (f, s) => f.negated != s.negated }) {
          None
        } else {
          unifyWithRename(leftLiterals.map(_.unit), rightLiterals.map(_.unit)).map {
            case (lmgu, rmgu) => (lmgu, rmgu, rmgu(right.conclusion.literals(desiredIndex)))
          }
        }
      })
      .find(_.isDefined)
      .flatten match {
      case None =>
        throw new IllegalArgumentException(
          "Unit-Propagation Resolution: given premise clauses are not resolvable")
      case Some((lmgu, rmgu, desired)) =>
        UnitPropagationResolution(left, right, desired, lmgu, rmgu)
    }
  }
}
