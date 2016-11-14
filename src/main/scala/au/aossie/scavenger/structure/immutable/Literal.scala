package au.aossie.scavenger.structure.immutable

import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.E

/**
  * Created by itegulov on 28.07.16.
  */
case class Literal(unit: E, negated: Boolean) {
  def unary_! = Literal(unit, !negated)

  def toClause: SeqClause = if (negated) new SeqClause(Seq(unit), Seq.empty) else new SeqClause(Seq.empty, Seq(unit))

  override def toString: String = if (negated) s"$unit ⊢" else s"⊢ $unit"
}
