package org.aossie.scavenger

import org.aossie.scavenger.structure.immutable.{Clause, Literal}
import org.aossie.scavenger.unification.{tools, MartelliMontanari => unify}
import org.aossie.scavenger.expression.substitution.immutable.Substitution
import org.aossie.scavenger.expression._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Random

/**
  * @author Daniyar Itegulov
  */
package object prover {
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

