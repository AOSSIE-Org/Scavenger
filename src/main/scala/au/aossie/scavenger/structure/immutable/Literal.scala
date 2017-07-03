package au.aossie.scavenger.structure.immutable

import au.aossie.scavenger.expression.E

/**
  * Created by itegulov on 28.07.16.
  */
// TODO: it would be more intuitive to rename 'negated' to 'polarity'
// and make 'polarity' equal to 'true' when the literal is positive
// and equal to 'false' when the literal is negative
case class Literal(unit: E, negated: Boolean) {
  def unary_! = Literal(unit, !negated)

  def toClause: Clause = if (negated) Clause(unit)() else Clause()(unit)

  def depth = unit.depth

  override def toString: String = if (negated) s"$unit ⊢" else s"⊢ $unit"

  override def hashCode(): Int = toString.hashCode
  override def equals(other: Any): Boolean = other match {
    case lit: Literal => toString == lit.toString
    case _ => false
  }
}
