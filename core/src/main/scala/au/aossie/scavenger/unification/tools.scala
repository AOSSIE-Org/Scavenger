package au.aossie.scavenger.unification

import au.aossie.scavenger.expression._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.unification.{MartelliMontanari => unify}

import scala.util.Random

/**
  * Created by vlad107 on 7/5/17.
  */
object tools {

  def disunifiableQuickCheck(left: E, right: E): Boolean = (left, right) match {
    case (Var(_), _) =>
      false
    case (_, Var(_)) =>
      false
    case (Sym(c1), Sym(c2)) =>
      c1 != c2
    case (App(_, _), Sym(_)) =>
      true
    case (Sym(_), App(_, _)) =>
      true
    case (AppRec(Sym(fun1), lArgs), AppRec(Sym(fun2), rArgs)) =>
      if ((fun1 != fun2) || (lArgs.size != rArgs.size))
        true
      else
        lArgs.zip(rArgs).exists { case (l, r) => disunifiableQuickCheck(l, r) }
    case _ =>
      false
  }

  // FIXME: do we really want to use e.variables? maybe we mean e.freeVariables ?

  /**
    * Rename quantified variables in left so that they don't intersect
    * with quantified variables in right. It's necessary for unification
    * to work correctly.
    *
    * @param left     where quantified variables should be renamed
    * @param usedVars already used variables
    * @return proper substitution to rename without variable collisions
    */
  def renameVars(left: E, usedVars: Set[Var]): Substitution = {
    // TODO: check that modifications done in this function due to Sym to Var refactoring did not intriduce bugs

    //val sharedVars = unifiableVars(left) intersect usedVars // Variables which should be renamed
    val sharedVars = left.variables.toSet intersect usedVars // TODO: do we really need to convert to set here?

    val kvs = for (v <- sharedVars) yield {
      val replacement = {
        var newVar = Var(v + Random.nextInt(1000).toString)
        while (usedVars contains newVar) {
          newVar = Var(newVar + Random.nextInt(1000).toString)
        }
        newVar
      }

      v -> replacement
    }

    new Substitution(kvs.toMap)
  }

  /**
    * Pairwise unification (zipped) with renaming of mutual variables.
    * Left expressions are considered to have different variables:
    *
    * If left(0) contains Var("X") and left(1) contains Var("X"), then left(1)'s variable will
    * be renamed to Var("X'").
    *
    * While right expressions have common variables.
    *
    * @param left  expressions to be unified, where variables are considered different
    * @param right expressions to be unified, where variables are common
    * @return Some(leftSubs, rightSub) where leftSubs contains substitution for all left expressions and rightSub
    *         is the signle substitution for all right expressions
    *         None if there is no substitution.
    */
  // TODO: This method should be moved to the unification package
  def unifyWithRename(left: Seq[E], right: Seq[E]): Option[(Seq[Substitution], Substitution)] = {
    if (left.zip(right).forall { case (x, y) => x == y }) {
      Some((Seq.fill(left.size)(Substitution.empty), Substitution.empty))
    } else if (left.zip(right).exists{ case (l, r) => tools.disunifiableQuickCheck(l, r)}) {
      None
    } else {
      var usedVars: Set[Var] = right map {
        _.variables
      } reduce {
        _ ++ _
      } toSet
      val newLeftWithSub = for (oneLeft <- left) yield {
        val substitution = renameVars(oneLeft, usedVars)
        val newLeft = substitution(oneLeft)
        usedVars ++= newLeft.variables
        (newLeft, substitution)
      }
      val newLeft = newLeftWithSub.map(_._1)
      val subs = newLeftWithSub.map(_._2)
      val unificationProblem = newLeft.zip(right)
      val unificationSubstitution = unify(unificationProblem)
      unificationSubstitution.map(s => {
        val unifiedSubs = subs.map(_ (s))
        (unifiedSubs, s)
      })
    }
  }
}
