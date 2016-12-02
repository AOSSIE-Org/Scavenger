package au.aossie.scavenger.structure
package immutable

import language.implicitConversions
import au.aossie.scavenger.expression.E

import au.aossie.scavenger.expression.formula.Neg

import scala.collection.mutable


/**
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
//FIXME: We'll stick with SeqClause for right now due to order issues
//@deprecated("Use SetClause instead", "Scavenger")
class SeqClause private (val ant: Seq[E], val suc: Seq[E]) extends AbstractClause with ClauseLike[SeqClause] {
  def +(f:E) = new SeqClause(ant, (suc :+ f).distinct)
  def +:(f:E) = new SeqClause((ant :+ f).distinct, suc)
  def -(f:E) =  new SeqClause(ant, suc.filterNot(_ == f))
  def -:(f:E) = new SeqClause(ant.filterNot(_ == f), suc)

  def union(that: AbstractClause) = new SeqClause((ant union that.ant.toSeq).distinct, (suc union that.suc.toSeq).distinct)
  def diff(that: AbstractClause) = new SeqClause(ant diff that.ant.toSeq, suc diff that.suc.toSeq)
  def intersect(that: AbstractClause) = new SeqClause(ant intersect that.ant.toSeq, suc intersect that.suc.toSeq)

  def map[R](antF: E => R, sucF: E => R): (Seq[R], Seq[R]) = (ant.map(antF), suc.map(sucF))
}

object SeqClause {
  def apply(ant: Seq[E], suc: Seq[E]) = new SeqClause(ant.distinct, suc.distinct)

  def empty: SeqClause = SeqClause(Seq.empty, Seq.empty)

  implicit def fromTraversable(s: TraversableOnce[E]): SeqClause = {
    val ant = new mutable.Stack[E]
    val suc = new mutable.Stack[E]
    for (f <- s) f match {
      case Neg(g) => ant.push(g)
      case _ => suc.push(f)
    }
    new SeqClause(ant,suc)
  }
}

