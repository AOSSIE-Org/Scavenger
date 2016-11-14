package au.aossie.scavenger.structure
package immutable

import language.implicitConversions

import au.aossie.scavenger.expression.E
import collection.mutable.Stack
import au.aossie.scavenger.expression.formula.Neg
  

/** 
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
class SeqClause(val ant: Seq[E], val suc: Seq[E]) extends AbstractClause with ClauseLike[SeqClause] { 
  def +(f:E) = new SeqClause(ant, suc :+ f)
  def +:(f:E) = new SeqClause(ant :+ f, suc)
  def -(f:E) =  new SeqClause(ant, suc.filterNot(_ == f))
  def -:(f:E) = new SeqClause(ant.filterNot(_ == f), suc)
 
  def union(that: AbstractClause) = new SeqClause(ant union that.ant.toSeq, suc union that.suc.toSeq)
  def diff(that: AbstractClause) = new SeqClause(ant diff that.ant.toSeq, suc diff that.suc.toSeq)
  def intersect(that: AbstractClause) = new SeqClause(ant intersect that.ant.toSeq, suc intersect that.suc.toSeq)  
  
  // ToDo: Think about what to do with these methods
  def -*(f:E) = new SeqClause(ant, suc.filterNot(_ eq f)) 
  def -*:(f:E) = new SeqClause(ant.filterNot(_ eq f), suc)  
  def --*(s:SeqClause) = new SeqClause(ant.filterNot(f => s.ant.exists(_ eq f)), suc.filterNot(f => s.suc.exists(_ eq f)))
  
  def literals: Seq[Literal] =
    ant.map(Literal(_, negated = true)) ++ suc.map(Literal(_, negated = false))

  // TODO: this is inefficient
  def apply(pos: Int): Literal = literals(pos)

  def first: Literal = apply(0)

  def last: Literal = apply(literals.length - 1)

  def isUnit: Boolean = { width == 1 }
}

object SeqClause {
  def apply(ant:E*)(suc:E*) = new SeqClause(ant, suc)
  
  def empty = SeqClause()()
 
  implicit def fromTraversable(s: TraversableOnce[E]) = {
    val ant = new Stack[E]; val suc = new Stack[E];
    for (f <- s) f match {
      case Neg(g) => ant.push(g)
      case _ => suc.push(f)
    }
    new SeqClause(ant,suc)
  } 
}

