package au.aossie.scavenger.structure.immutable

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.E

/**
  * Created by itegulov on 28.07.16.
  */
// TODO: it would be more intuitive to rename 'negated' to 'polarity'
// and make 'polarity' equal to 'true' when the literal is positive
// and equal to 'false' when the literal is negative
case class Literal(unit: E, negated: Boolean) {
  def unary_! = Literal(unit, !negated)

  def toClause: SetClause = if (negated) new SetClause(Set(unit), Set.empty) else new SetClause(Set.empty, Set(unit))

  override def toString: String = if (negated) s"$unit ⊢" else s"⊢ $unit"
}

