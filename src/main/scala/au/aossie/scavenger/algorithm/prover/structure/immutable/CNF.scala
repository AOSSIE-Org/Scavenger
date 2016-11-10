package au.aossie.scavenger.algorithm.prover.structure.immutable

import au.aossie.scavenger.algorithm.prover._
import au.aossie.scavenger.algorithm.prover.Clause
import au.aossie.scavenger.algorithm.prover.structure.mutable

import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
case class CNF(clauses: Seq[Clause]) {
  lazy val variables = clauses.flatMap(_.literals.map(_.unit))

  def +(that: CNF): CNF = new CNF(clauses ++ that.clauses)

  def -(that: CNF): CNF = new CNF(clauses.filterNot(that.clauses.toSet))

  def toMutableCNF: mutable.CNF = new mutable.CNF(clauses.to[ArrayBuffer])
}
