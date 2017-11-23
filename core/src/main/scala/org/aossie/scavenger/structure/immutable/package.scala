package org.aossie.scavenger.structure

import org.aossie.scavenger.expression.E

package object immutable {
  implicit class LiteralsAreSequent(val literals: Iterable[Literal]) extends AnyVal {
    def toClause: Clause = {
      val (ant, suc) = literals.partition(_.polarity)
      Clause(suc.map(_.unit).toSeq: _*)(ant.map(_.unit).toSeq: _*)
    }
  }

  implicit def varToLit(variable: E): Literal = Literal(variable, polarity = true)

  implicit def literalToClause(literal: Literal): Clause = literal.toClause
}
