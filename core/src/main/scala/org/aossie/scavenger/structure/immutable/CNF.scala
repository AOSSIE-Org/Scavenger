package org.aossie.scavenger.structure.immutable

import org.aossie.scavenger.expression.Sym
import org.aossie.scavenger.structure.mutable

import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
// TODO: it seems odd to have CNF as a case class, because CNF is like a collection
// and collections are typically not case classes.
case class CNF(clauses: Seq[Clause]) {
  lazy val variables = clauses.flatMap(_.literals.map(_.unit))
  lazy val predicates = clauses.flatMap(_.predicates).distinct
  lazy val functionSymbols = clauses.flatMap(_.functionSymbols).distinct
  lazy val constantSymbols: Set[Sym] = clauses.flatMap(_.constantSymbols).toSet

  def +(that: CNF): CNF = CNF(clauses ++ that.clauses)

  def -(that: CNF): CNF = CNF(clauses.filterNot(that.clauses.toSet))

  def toMutableCNF: mutable.CNF = new mutable.CNF(clauses.to[ArrayBuffer])
}

