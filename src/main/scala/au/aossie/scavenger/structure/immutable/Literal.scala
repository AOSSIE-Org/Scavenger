package au.aossie.scavenger.structure.immutable

import au.aossie.scavenger.expression.E

/**
  * Created by itegulov on 28.07.16.
  */
case class Literal(unit: E, polarity: Boolean) {
  def unary_! = Literal(unit, !polarity)

  def toClause: Clause = if (!polarity) Clause(unit)() else Clause()(unit)

  def depth = unit.depth

  override def toString: String = if (!polarity) s"$unit ⊢" else s"⊢ $unit"

  override def hashCode(): Int = toString.hashCode
  override def equals(other: Any): Boolean = other match {
    case lit: Literal => toString == lit.toString
    case _ => false
  }
}
