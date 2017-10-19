package org.aossie.scavenger.structure.immutable

import org.aossie.scavenger.expression.E

/**
  * Created by itegulov on 28.07.16.
  */
case class Literal(unit: E, polarity: Boolean) {
  def unary_! = Literal(unit, !polarity)

  def toClause: Clause = if (!polarity) Clause(unit)() else Clause()(unit)

  def depth = unit.depth

  override def toString: String = if (!polarity) s"$unit ⊢" else s"⊢ $unit"
}
