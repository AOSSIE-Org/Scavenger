package au.aossie.scavenger.structure.immutable

import au.aossie.scavenger.structure.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
// TODO: it seems odd to have CNF as a case class, because CNF is like a collection
// and collections are typically not case classes.
case class CNF(clauses: Seq[Clause]) {
  lazy val variables = clauses.flatMap(_.literals.map(_.unit))
  lazy val predicates = Set(clauses.flatMap(_.predicates): _*).toSeq
  lazy val functionSymbols = Set(clauses.flatMap(_.functionSymbols): _*).toSeq

  def +(that: CNF): CNF = CNF(clauses ++ that.clauses)

  def -(that: CNF): CNF = CNF(clauses.filterNot(that.clauses.toSet))

  def toMutableCNF: mutable.CNF = new mutable.CNF(clauses.to[ArrayBuffer])
}

