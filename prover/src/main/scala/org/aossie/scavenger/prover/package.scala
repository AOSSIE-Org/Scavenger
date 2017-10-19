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
}

