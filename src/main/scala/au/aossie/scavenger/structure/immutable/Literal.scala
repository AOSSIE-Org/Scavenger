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

  def toClause: SeqClause = if (negated) SeqClause(Seq(unit), Seq.empty) else SeqClause(Seq.empty, Seq(unit))

  override def toString: String = if (negated) s"$unit ⊢" else s"⊢ $unit"
}

