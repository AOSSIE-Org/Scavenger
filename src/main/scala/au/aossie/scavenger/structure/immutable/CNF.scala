package au.aossie.scavenger.structure.immutable

import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.structure.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
// TODO: it seems odd to have CNF as a case class, because CNF is like a collection
// and collections are typically not case classes.
case class CNF(clauses: Seq[SeqClause]) {
  lazy val variables = clauses.flatMap(_.literals.map(_.unit))

  def +(that: CNF): CNF = new CNF(clauses ++ that.clauses)

  def -(that: CNF): CNF = new CNF(clauses.filterNot(that.clauses.toSet))

  def toMutableCNF: mutable.CNF = new mutable.CNF(clauses.to[ArrayBuffer])
}

