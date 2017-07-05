package au.aossie.scavenger

import au.aossie.scavenger.structure.immutable.{Literal, Clause}
import au.aossie.scavenger.unification.{MartelliMontanari => unify}
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression._

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Daniyar Itegulov
  */
package object prover {

  implicit def varToLit(variable: E): Literal = Literal(variable, polarity = true)

  implicit def literalToClause(literal: Literal): Clause = literal.toClause

  //  implicit class ClauseOperations(val clause: Clause) extends AnyVal {
  //
  //  }

  implicit class UnitSequent(val sequent: Clause) extends AnyVal {
    def literal: Literal =
      if (sequent.ant.size == 1 && sequent.suc.isEmpty) Literal(sequent.ant.head, polarity = false)
      else if (sequent.ant.isEmpty && sequent.suc.size == 1) Literal(sequent.suc.head, polarity = true)
      else throw new IllegalStateException("Given SeqSequent is not a unit")
  }

  implicit class LiteralsAreSequent(val literals: Iterable[Literal]) extends AnyVal {
    def toClause: Clause = {
      val ant = literals.flatMap(l => if (!l.polarity) Some(l.unit) else None)
      val suc = literals.flatMap(l => if (!l.polarity) None else Some(l.unit))
      Clause(ant.toSeq: _*)(suc.toSeq: _*)
    }
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
  def renameVars(left: E, usedVars: mutable.Set[Var]): Substitution = {
    // TODO: check that modifications done in this function due to Sym to Var refactoring did not intriduce bugs

    //val sharedVars = unifiableVars(left) intersect usedVars // Variables which should be renamed
    val sharedVars = left.variables.toSet intersect usedVars // TODO: do we really need to convert to set here?

    val kvs = for (v <- sharedVars) yield {
      val replacement = {
        var newVar = Var(v + "'")
        while (usedVars contains newVar) {
          newVar = Var(newVar + "'")
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

  def bad(left: E, right: E): Boolean = (left, right) match {
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
    case (App(f1: Sym, _), App(f2: Sym, _)) if f1 != f2 =>
      true
    case (AppRec(_: Sym, lArgs), AppRec(_: Sym, rArgs)) if lArgs.size != rArgs.size =>
      true
    case (AppRec(_: Sym, lArgs), AppRec(_: Sym, rArgs)) =>
//      false
      lArgs.zip(rArgs).exists { case (l, r) => bad(l, r) }
    case _ =>
      false
  }

  // TODO: This method should be moved to the unification package
  def unifyWithRename(left: Seq[E], right: Seq[E]): Option[(Seq[Substitution], Substitution)] = {
    if (left.zip(right).forall { case (x, y) => x == y }) {
      Some(Seq.fill(left.size)(Substitution.empty), Substitution.empty)
    } else if (left.zip(right).exists{ case (l, r) => bad(l, r)}) {
      None
    } else {
      var usedVars: mutable.Set[Var] = mutable.Set(right map {
        _.variables
      } reduce {
        _ ++ _
      }: _*)
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

  /**
    * Computes all combinations of list of lists.
    * Example: combinations(Seq(Seq(1, 2), Seq(1, 3))) == Seq(Seq(1, 1), Seq(1, 3), Seq(2, 1), Seq(2, 3))
    *
    * @param xss sequence of sequences of possible elements
    * @tparam A type of elements
    * @return all possible combinations of elements
    */
  def combinations[A](xss: Seq[Seq[A]]): Seq[Seq[A]] =
    xss.foldLeft(Seq(Seq.empty[A])) { (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def tptpPrettify(e: E): String = {
    e match {
      case Abs(_, _, _) => throw new IllegalArgumentException("Doesn't work with abs")
      case Sym("additive_identity") => "0"
      case Sym("multiplicative_identity") => "1"
      case App(Sym("multiplicative_inverse"), a) => "1/" + tptpPrettify(a)
      case App(Sym("additive_inverse"), a) => "-" + tptpPrettify(a)
      case AppRec(Sym("less_or_equal"), Seq(l, r)) => tptpPrettify(l) + " ≤ " + tptpPrettify(r)
      case AppRec(Sym("sum"), Seq(a, b, c)) => tptpPrettify(a) + " + " + tptpPrettify(b) + " = " + tptpPrettify(c)
      case AppRec(Sym("add"), Seq(a, b)) => tptpPrettify(a) + " + " + tptpPrettify(b)
      case AppRec(Sym("product"), Seq(a, b, c)) => tptpPrettify(a) + " * " + tptpPrettify(b) + " = " + tptpPrettify(c)
      case AppRec(Sym("multiply"), Seq(a, b)) => tptpPrettify(a) + " * " + tptpPrettify(b)
      case x => x.toString
    }
  }

  def tptpPrettify(clause: Clause): String = {
    val (ant, suc) = clause.map(tptpPrettify, tptpPrettify)
    ant.map("¬" + _).mkString(" ∨ ") + " ⊢ " + suc.mkString(" ∨ ")
  }
}

