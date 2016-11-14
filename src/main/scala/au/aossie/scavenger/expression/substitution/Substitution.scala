package au.aossie.scavenger.expression
package substitution

import au.aossie.scavenger.structure.immutable.Literal

//TODO: (B) Take care of bound variable renaming, in order to avoid variable capture. 
abstract class AbstractSubstitution {
  protected def m: Map[Sym,E]
  def apply(e: E) = {
    def rec(f:E,boundVars:Set[Sym]):E = f match {
      case App(e1, e2) => App(rec(e1,boundVars),rec(e2,boundVars))
      case Abs(v,e) => Abs(v, rec(e, boundVars + v))
      case v: Sym if (boundVars contains v) => v 
      case v: Sym if (m contains v) => m(v)
      case v: Sym => v
    }
    rec(e, Set())
  }

  def apply(literal: Literal): Literal = Literal(apply(literal.unit), literal.negated)
}