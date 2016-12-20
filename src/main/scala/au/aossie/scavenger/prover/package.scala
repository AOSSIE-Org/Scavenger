package au.aossie.scavenger

import au.aossie.scavenger.structure.immutable.{ Literal, SetClause => Clause }
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

  implicit def literalToClause(literal: Literal): Clause = literal.toSetClause

//  implicit class ClauseOperations(val clause: Clause) extends AnyVal {
//
//  }

  implicit class UnitSequent(val sequent: Clause) extends AnyVal {
    def literal: Literal =
      if (sequent.ant.size == 1 && sequent.suc.isEmpty) Literal(sequent.ant.head, negated = true)
      else if (sequent.ant.isEmpty && sequent.suc.size == 1) Literal(sequent.suc.head, negated = false)
      else throw new IllegalStateException("Given SeqSequent is not a unit")
  }

  implicit class LiteralsAreSequent(val literals: Iterable[Literal]) extends AnyVal {
    def toSequent: Clause = {
      val ant = literals.flatMap(l => if (l.negated) Some(l.unit) else None)
      val suc = literals.flatMap(l => if (l.negated) None else Some(l.unit))
      Clause(ant.toSeq: _*)(suc.toSeq: _*)
    }
  }

  // FIXME: do we really want to use e.variables? maybe we mean e.freeVariables ?

  /**
    * Rename quantified variables in left so that they don't intersect
    * with quantified variables in right. It's necessary for unification
    * to work correctly.
    *
    * @param left where quantified variables should be renamed
    * @param usedVars already used variables
    * @return proper substitution to rename without variable collisions
    */
  def renameVars(left: E, usedVars: Set[Var]): Substitution = {
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
    * @param left expressions to be unified, where variables are considered different
    * @param right expressions to be unified, where variables are common
    * @return Some(leftSubs, rightSub) where leftSubs contains substitution for all left expressions and rightSub
    *           is the signle substitution for all right expressions
    *         None if there is no substitution.
    */
  def unifyWithRename(left: Seq[E], right: Seq[E]): Option[(Seq[Substitution], Substitution)] = {
    var usedVars = right map { _.variables.toSet } reduce { _ union _ }
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
    val usedVars = what.variables.toSet
    val sub = renameVars(from, usedVars)
    val newFrom = sub(from)
    val newVars = what.variables.toSet
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

  def tptpPrettify(clause: Clause): String = {
    val (ant, suc) = clause.map(tptpPrettify, tptpPrettify)
    ant.map("¬" + _).mkString(" ∨ ") + " ⊢ " + suc.mkString(" ∨ ")
  }
}

