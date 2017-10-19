package org.aossie.scavenger.preprocessing

import org.aossie.scavenger.expression.{AppRec, Infix, Sym, Var}
import org.aossie.scavenger.structure.immutable.Clause
import com.typesafe.scalalogging.Logger

import scala.collection.mutable

/**
  * Created by vlad107 on 7/12/17.
  */
object AddEqualityReasoningAxioms {
  def add(clauses: mutable.ListBuffer[Clause])(implicit logger: Logger) = {
    /**
      * symmetry axiom
      */
    clauses.append(
      Clause(AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("B"))))
      (AppRec(new Sym("=") with Infix, Seq(Var("B"), Var("A")))))

    /**
      * reflexivity axiom
      */
    clauses.append(Clause()(AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("A")))))

    /**
      * transitivity axiom
      */
    clauses.append(
      Clause(AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("B"))), AppRec(new Sym("=") with Infix, Seq(Var("B"), Var("C"))))
      (AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("C")))))

    /** congruence axioms for predicates
      */
    clauses.flatMap(_.predicates).distinct.foreach { case (predicate, arity) =>
      // TODO: add support for airty > 13
      if (2 * arity <= 26) {
        val leftVariables =
          List.range('A'.toInt, 'A'.toInt + arity).map(ind => Var(ind.toChar.toString))
        val rightVariables =
          List.range('A'.toInt + arity, 'A'.toInt + 2 * arity).map(ind => Var(ind.toChar.toString))
        val equalities = leftVariables.zip(rightVariables).map {
          case (left, right) => AppRec(new Sym("=") with Infix, Seq(left, right))
        }
        val predicateOnLeft = AppRec(predicate, leftVariables)
        val predicateOnRight = AppRec(predicate, rightVariables)
        val congAxiom = Clause(equalities :+ predicateOnLeft: _*)(predicateOnRight)
        clauses.append(congAxiom)
      } else {
        logger.warn(s"predicates with arity(=$arity) > 13 not supported")
      }
    }

    /** congruence axioms for functions
      */
    val funs = clauses.flatMap(_.functionSymbols).distinct.filter(_._2 > 0)
    funs.foreach { case (fun, arity) =>
      // TODO: for arity > 13
      if (2 * arity <= 26) {
        val leftVariables =
          List.range('A'.toInt, 'A'.toInt + arity).map(ind => Var(ind.toChar.toString))
        val rightVariables =
          List.range('A'.toInt + arity, 'A'.toInt + 2 * arity).map(ind => Var(ind.toChar.toString))
        val equalities = leftVariables.zip(rightVariables).map {
          case (left, right) => AppRec(new Sym("=") with Infix, Seq(left, right))
        }
        val leftFun = AppRec(fun, leftVariables)
        val rightFun = AppRec(fun, rightVariables)
        val predicateOnRight = AppRec(Sym("="), Seq(leftFun, rightFun))
        val congAxiom = Clause(equalities: _*)(predicateOnRight)
        clauses.append(congAxiom)
      } else {
        logger.warn(s"functions with arity(=$arity) > 13 not supported")
      }
    }

  }
}
