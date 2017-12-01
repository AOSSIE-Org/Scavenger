package org.aossie.scavenger.structure.immutable

import org.aossie.scavenger.expression.{AppRec, E, Sym}

/**
 * @author Daniyar Itegulov
 */
case class Literal(unit: E, polarity: Boolean) {
  val arguments: List[E] =
    unit match {
      case AppRec(_, args) => args.toList
    }

  lazy val predicate: (Sym, Int) =
    unit match {
      case AppRec(predicateSymbol: Sym, args) => (predicateSymbol, args.size)
    }

  def unary_! = Literal(unit, !polarity)

  def toClause: Clause = if (!polarity) Clause(unit)() else Clause()(unit)

  def depth = unit.depth

  override def toString: String = if (!polarity) s"$unit ⊢" else s"⊢ $unit"
}
