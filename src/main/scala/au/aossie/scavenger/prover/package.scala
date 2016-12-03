package au.aossie.scavenger

import au.aossie.scavenger.structure.immutable.{ Literal, SeqClause }
import au.aossie.scavenger.unification.{ MartelliMontanari => unify }
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression._

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Daniyar Itegulov
  */
package object prover {

  implicit def varToLit(variable: E): Literal = Literal(variable, negated = false)

  implicit def literalToClause(literal: Literal): SeqClause = literal.toClause

//  implicit class ClauseOperations(val clause: Clause) extends AnyVal {
//
//  }

  implicit class UnitSequent(val sequent: SeqClause) extends AnyVal {
    def literal: Literal =
      if (sequent.ant.size == 1 && sequent.suc.isEmpty) Literal(sequent.ant.head, negated = true)
      else if (sequent.ant.isEmpty && sequent.suc.size == 1) Literal(sequent.suc.head, negated = false)
      else throw new IllegalStateException("Given SeqSequent is not a unit")
  }

  implicit class LiteralsAreSequent(val literals: Iterable[Literal]) extends AnyVal {
    def toSequent: SeqClause = {
      val ant = literals.flatMap(l => if (l.negated) Some(l.unit) else None)
      val suc = literals.flatMap(l => if (l.negated) None else Some(l.unit))
      SeqClause(ant.toSeq, suc.toSeq)
    }
  }

  /**
    * Gets all unifiable variables (contained in `variables`) from
    * given Es.
    *
    * @param exps where unifiable variables should be found
    * @return unifiable variables contained at least once in exps
    */
  def unifiableVars(exps: E*)(implicit variables: mutable.Set[Sym]): Set[Sym] = exps.flatMap {
    case App(e1, e2) =>
      unifiableVars(e1) union unifiableVars(e2)
    case Abs(v, t, e1) =>
      unifiableVars(v) union unifiableVars(e1)
    case v: Sym =>
      if (variables contains v) Set(v) else Set.empty[Sym]
  }.toSet

  /**
    * Rename quantified variables in left so that they don't intersect
    * with quantified variables in right. It's necessary for unification
    * to work correctly.
    *
    * @param left where quantified variables should be renamed
    * @param usedVars already used variables
    * @return proper substitution to rename without variable collisions
    */
  def renameVars(left: E, usedVars: Set[Sym])(implicit variables: mutable.Set[Sym]): Substitution = {
    val sharedVars = unifiableVars(left) intersect usedVars // Variables which should be renamed

    // Unification variables which can be reused for new variables
    val notUsedVars = variables diff (sharedVars union unifiableVars(left) union usedVars)

    val kvs = for (v <- sharedVars) yield {
      val replacement = notUsedVars.headOption getOrElse { // Use some variable from unification variables
      // Or create a new one
      var newVar = Sym(v + "'") // Critical point where type information was lost during refactoring
        while (usedVars contains newVar) {
          newVar = Sym(newVar + "'") // Critical point where type information was lost during refactoring
        }
        variables += newVar // It will be available for unification from now
        newVar
      }

      if (notUsedVars contains replacement) notUsedVars -= replacement
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
    * @param left expressions to be unified, where variables are considered different
    * @param right expressions to be unified, where variables are common
    * @return Some(leftSubs, rightSub) where leftSubs contains substitution for all left expressions and rightSub
    *           is the signle substitution for all right expressions
    *         None if there is no substitution.
    */
  def unifyWithRename(left: Seq[E], right: Seq[E])
                     (implicit variables: mutable.Set[Sym]): Option[(Seq[Substitution], Substitution)] = {
    var usedVars = unifiableVars(right: _*)
    val newLeftWithSub = for (oneLeft <- left) yield {
      val substitution = renameVars(oneLeft, usedVars)
      val newLeft = substitution(oneLeft)
      usedVars ++= unifiableVars(newLeft)
      (newLeft, substitution)
    }
    val newLeft = newLeftWithSub.map(_._1)
    val subs = newLeftWithSub.map(_._2)
    val unificationProblem = newLeft.zip(right)
    val unificationSubstitution = unify(unificationProblem)
    unificationSubstitution.map(s => {
      val unifiedSubs = for (renameSubstitution <- subs) yield {
        val unificationRenamedSubstitution = for ((key, value) <- renameSubstitution) yield (key, s(value))
        val left = s.filterNot { case (k, v) => renameSubstitution.contains(k) }
        new Substitution(unificationRenamedSubstitution ++ left)
      }
      (unifiedSubs, s)
    })
  }

  /**
    * Checks if there is such unification, which don't use unification variables from `what`.
    *
    * @param what what should be instantiated
    * @param from from what should be instantiated
    * @param variables unifiaction variables
    * @return true, if there exists some unification for what and from according to rules
    *         false, otherwise
    */
  def isInstantiation(what: E, from: E)(implicit variables: mutable.Set[Sym]): Boolean = {
    val usedVars = unifiableVars(what)
    val sub = renameVars(from, usedVars)
    val newFrom = sub(from)
    val newVars = unifiableVars(what)
    variables --= newVars // newVars are fixed
    val result = unify((what, newFrom) :: Nil) match {
      case None => false
      case Some(_) => true
    }
    variables ++= newVars // Should revert variables set back to initial state
    result
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

  def tptpPrettify(clause: SeqClause): String = {
    val (ant, suc) = clause.map(tptpPrettify, tptpPrettify)
    ant.map("¬" + _).mkString(" ∨ ") + " ⊢ " + suc.mkString(" ∨ ")
  }
}

