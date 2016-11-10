package au.aossie.scavenger.proof.sequent.conflictresolution

import au.aossie.scavenger.algorithm.prover._
import au.aossie.scavenger.algorithm.prover.structure.immutable.Literal
import au.aossie.scavenger.expression.Var
import au.aossie.scavenger.judgment.immutable.SeqSequent
import au.aossie.scavenger.proof.sequent.SequentProofNode

import scala.collection.mutable

/**
  * Represents Unit-Propagation Resolution rule from Conflict Resolution calculus.
  *
  * @author Daniyar Itegulov
  */
case class UnitPropagationResolution(left: Seq[SequentProofNode], right: SequentProofNode, desired: Literal)
                                    (implicit variables: mutable.Set[Var]) extends SequentProofNode {
  require(left.forall(_.conclusion.width == 1), "All left conclusions should be unit")
  require(left.size + 1 == right.conclusion.width, "There should be enough left premises to derive desired")
  val leftLiterals = left.map(_.conclusion.literals.head)
  val (leftMgus, rightMgu, rightLiterals, desiredIndex) = right.conclusion.literals.indices.map(desiredIndex => {
    // Find such desired index that remaining right literals will be unifiable with left literals
    val rightLiterals = right.conclusion.literals.patch(desiredIndex, Nil, 1)
    if (!leftLiterals.zip(rightLiterals).forall { case (f, s) => f.negated != s.negated }) {
      None
    } else {
      val leftAux = leftLiterals.map(_.unit)
      val rightAux = rightLiterals.map(_.unit)

      unifyWithRename(leftAux, rightAux).flatMap {
        case (lmgu, rmgu) =>
          val conclusion = rmgu(right.conclusion.literals(desiredIndex))
          if (conclusion.negated != desired.negated || !isInstantiation(desired.unit, conclusion.unit)) {
            None // If desired literal is not a resulting literal
          } else {
            Some(lmgu, rmgu, rightLiterals, desiredIndex) // Pack some additional information to Option
          }
      }
    }
  }).find(_.isDefined).flatten match {
    case None => throw new IllegalArgumentException("Unit-Propagation Resolution: given premise clauses are not resolvable")
    case Some(u) => u
  }

  override def auxFormulasMap: Map[SequentProofNode, SeqSequent] =
    (left.map(spn => spn -> spn.conclusion) :+ (right -> rightLiterals.toSequent.toSeqSequent)).toMap

  override def conclusionContext: SeqSequent = rightMgu(right.conclusion.literals(desiredIndex)).toSeqSequent

  override def mainFormulas: SeqSequent = SeqSequent()()

  override def premises: Seq[SequentProofNode] = left :+ right
}

object UnitPropagationResolution {
  def apply(left: Seq[SequentProofNode], right: SequentProofNode)
           (implicit variables: mutable.Set[Var]): UnitPropagationResolution = {
    val leftLiterals = left.map(_.conclusion.literals.head)
    right.conclusion.literals.indices.map(desiredIndex => { // FIXME: copy-pasted code
      // Find such desired index that remaining right literals will be unifiable with left literals
      val rightLiterals = right.conclusion.literals.patch(desiredIndex, Nil, 1)
      if (!leftLiterals.zip(rightLiterals).forall { case (f, s) => f.negated != s.negated }) {
        None
      } else {
        unifyWithRename(leftLiterals.map(_.unit), rightLiterals.map(_.unit)).map {
          case (lmgu, rmgu) => rmgu(right.conclusion.literals(desiredIndex))
        }
      }
    }).find(_.isDefined).flatten match {
      case None => throw new IllegalArgumentException("Unit-Propagation Resolution: given premise clauses are not resolvable")
      case Some(desired) => UnitPropagationResolution(left, right, desired)
    }
  }
}