package org.aossie.scavenger.parser

import org.aossie.scavenger.expression.{AppRec, E, Sym, T, Var}

/**
 * Objects to facilitate the creation of logical terms.
 *
 * @author Ezequiel Postan
 * @since 24.05.2016
 */
abstract class Symbol {
  def apply(name: String): E       = Sym(name)
  def apply(name: String, i: T): E = Sym(name)
}

object DistinctObjectTerm extends Symbol

object NumberTerm extends Symbol

object Constant extends Symbol

object Variable extends Symbol {
  override def apply(name: String): E       = Var(name)
  override def apply(name: String, t: T): E = Var(name)
}

object FunctionTerm {
  def apply(name: String, args: List[E]): E = AppRec(Sym(name), args)
  def unapply(e: E) = e match {
    case AppRec(f, args) if args.nonEmpty => Some((f, args))
    case _                                => None
  }
}

object TypedNumberTerm extends Symbol

object TypedConstant extends Symbol

object TypedVariable extends Symbol {
  override def apply(name: String): E       = Var(name)
  override def apply(name: String, t: T): E = Var(name)
}

object TypedFunctionSymbol extends Symbol

object ConditionalTerm {
  val conditional = "conditionalTerm"
  val ifThenElse  = Sym(conditional)
  def apply(condition: E, t1: E, t2: E): E =
    AppRec(ifThenElse, List(condition, t1, t2))
}
