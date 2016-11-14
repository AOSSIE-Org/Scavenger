package au.aossie.scavenger.prover.structure.mutable

import au.aossie.scavenger.prover._
import au.aossie.scavenger.structure.immutable.{Literal,SeqClause}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Represents propositional CNF.
  *
  * @author Daniyar Itegulov
  */
class CNF(val clauses: ArrayBuffer[SeqClause]) {
  /**
    * Shows which literals are set to be true.
    */
  val assignment = mutable.Set.empty[Literal]

  /**
    * Just all variables, contained in CNF.
    */
  val variables = clauses.flatMap(_.literals.map(_.unit))

  /**
    * Represents two-watched literal scheme:
    * for each literal we know what clauses have watchers set
    * to this literal.
    */
  val sentinels: Map[Literal, mutable.Set[SeqClause]] = {
    val sentinels = variables.flatMap(variable =>
      Seq(
        varToLit(variable) -> mutable.Set.empty[SeqClause],
        !varToLit(variable) -> mutable.Set.empty[SeqClause]
      )
    ).toMap
    for (clause <- clauses) if (clause.width >= 2) {
      sentinels(clause.first) += clause
      sentinels(clause.last) += clause
    }
    sentinels
  }

  def +=(that: SeqClause): CNF = {
    if (that.width >= 1) {
      sentinels(that.first) += that
      sentinels(that.last) += that
    }
    clauses += that
    this
  }

  def -=(that: SeqClause): CNF = {
    if (clauses.contains(that) && that.width >= 1) {
      sentinels(that.first) -= that
      sentinels(that.last) -= that
    }
    clauses -= that
    this
  }

  private def clauseIsSatisfied(clause: SeqClause): Boolean = clause.literals.exists(assignment.contains)

  /**
    * Ensures that provided literal is true and returns sequence
    * of literals that should also be true.
    *
    * @param literal which should be true
    * @return sequence of literals that also should be true
    */
  def assignLiteral(literal: Literal): Seq[Literal] = {
    assignment += literal
    val result = ArrayBuffer.empty[Literal]
    for (clause <- sentinels(!literal)) if (!clauseIsSatisfied(clause)) {
      val otherLiterals = clause.literals.filter(_ != !literal)
      otherLiterals.find(!sentinels(_).contains(clause)) match {
        case Some(nonSentinelLiteral) =>
          sentinels(!literal) -= clause
          sentinels(nonSentinelLiteral) += clause
        case None =>
          otherLiterals.find(sentinels(_) contains clause) match {
            case Some(sentinel) => result += sentinel
            case None =>
          }
      }
    }
    result
  }
}
