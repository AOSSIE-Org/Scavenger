package org.aossie.scavenger.structure

import org.aossie.scavenger.expression.E

package object immutable {
  implicit class LiteralsAreSequent(val literals: Iterable[Literal]) extends AnyVal {
    def toClause: Clause = {
      val ant = literals.flatMap(l => if (!l.polarity) Some(l.unit) else None)
      val suc = literals.flatMap(l => if (!l.polarity) None else Some(l.unit))
      Clause(ant.toSeq: _*)(suc.toSeq: _*)
    }
  }

  implicit def varToLit(variable: E): Literal = Literal(variable, polarity = true)

  implicit def literalToClause(literal: Literal): Clause = literal.toClause
}
