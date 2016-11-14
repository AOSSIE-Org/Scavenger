package au.aossie.scavenger.structure.immutable

import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.structure.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
case class CNF(clauses: Seq[SeqClause]) {
  lazy val variables = clauses.flatMap(_.literals.map(_.unit))

  def +(that: CNF): CNF = new CNF(clauses ++ that.clauses)

  def -(that: CNF): CNF = new CNF(clauses.filterNot(that.clauses.toSet))

  def toMutableCNF: mutable.CNF = new mutable.CNF(clauses.to[ArrayBuffer])
}
