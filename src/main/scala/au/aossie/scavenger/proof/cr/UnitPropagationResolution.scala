package au.aossie.scavenger.proof.cr

import au.aossie.scavenger.prover._
import au.aossie.scavenger.structure.immutable.{ Literal, Clause }
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.expression.substitution.immutable.Substitution

import scala.collection.mutable

/**
  * Represents Unit-Propagation Resolution rule from Conflict Resolution calculus.
  *
  * @author Daniyar Itegulov
  */
class UnitPropagationResolution (
    val left: Seq[CRProofNode],
    val right: CRProofNode,
    val desired: Literal,
    val leftMgus: Seq[Substitution],
    val rightMgu: Substitution)
    extends CRProofNode(
      left.forall(_.isAxiom) & right.isAxiom,
      left.zip(leftMgus).flatMap {
        case  (node, sub) =>
          node.decisions.map(literal => sub(literal))
      }(collection.breakOut),
      left.zip(leftMgus).flatMap {
        case (node, sub) =>
          node.nonExpertDecisions.map(literal => sub(literal))
      }(collection.breakOut).toSet ++ right.nonExpertDecisions.map(literal => rightMgu(literal)),
      left.flatMap(_.decisionsWithoutSubst)(collection.breakOut).toSet ++
        right.decisionsWithoutSubst
    ) {
  assert(left.forall(_.conclusion.width == 1), "All left conclusions should be unit")
  assert(left.size + 1 == right.conclusion.width,
          "There should be enough left premises to derive desired")

  override def conclusion: Clause = desired
  override def premises: Seq[CRProofNode] = left :+ right
}

object UnitPropagationResolution {
  def apply(left: Seq[CRProofNode], right: CRProofNode, desired: Literal, desiredRightLiterals: Seq[Literal]): UnitPropagationResolution = {
    val leftLiterals = left.map(_.conclusion.literals.head)
    // Find such desired index that remaining right literals will be unifiable with left literals
    val rightLiterals = right.conclusion.literals.filterNot(_ == desired)
    assert(desiredRightLiterals.forall(rightLiterals.contains))
    assert(rightLiterals.forall(desiredRightLiterals.contains))
    if (!leftLiterals.zip(desiredRightLiterals).forall { case (f, s) => f.polarity != s.polarity }) {
      throw new IllegalArgumentException("Left literals and right clause aren't unifiable")
    } else {
      unifyWithRename(leftLiterals.map(_.unit), desiredRightLiterals.map(_.unit)) match {
        case Some((leftMgus, rightMgu)) =>
          val newDesired = rightMgu(desired)
          new UnitPropagationResolution(left, right, newDesired, leftMgus, rightMgu)
        case _ =>
          throw new IllegalArgumentException("Left literals and right clause aren't unifiable")
      }
    }
  }
  def apply(left: Seq[CRProofNode],
            right: CRProofNode,
            desired: Literal,
            desiredRightLiterals: Seq[Literal],
            subst: Seq[Substitution],
            globalSubst: Substitution): UnitPropagationResolution = {
    val leftLiterals = left.map(_.conclusion.literals.head)
    // Find such desired index that remaining right literals will be unifiable with left literals
    val rightLiterals = right.conclusion.literals.filterNot(_ == desired)
    assert(desiredRightLiterals.forall(rightLiterals.contains))
    assert(rightLiterals.forall(desiredRightLiterals.contains))
    if (!leftLiterals.zip(desiredRightLiterals).forall { case (f, s) => f.polarity != s.polarity }) {
      throw new IllegalArgumentException("Left literals and right clause aren't unifiable")
    } else {
      new UnitPropagationResolution(left, right, globalSubst(desired), subst, globalSubst)
    }
  }
  def unapply(p: CRProofNode) = p match {
    case p: UnitPropagationResolution => Some((p.left, p.right, p.desired, p.leftMgus, p.rightMgu))
    case _ => None
  }
}
