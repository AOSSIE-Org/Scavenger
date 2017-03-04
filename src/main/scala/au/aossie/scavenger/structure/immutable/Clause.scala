package au.aossie.scavenger.structure
package immutable

import au.aossie.scavenger.expression.E

import scala.collection.immutable.ListSet

/**
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.1
 *  @since   0.1
 */
class Clause(val ant: ListSet[E], val suc: ListSet[E]) extends ClauseLike[Clause] {
  def literals: Seq[Literal] = ant.toSeq.map(Literal(_, negated = true)) ++ suc.toSeq.map(Literal(_, negated = false))

  def +(f:E) = new Clause(ant, suc + f)
  def +:(f:E) = new Clause(ant + f, suc)
  def -(f:E) =  new Clause(ant, suc - f)
  def -:(f:E) = new Clause(ant - f, suc)

  def union(that: Clause) = new Clause(ant union that.ant, suc union that.suc)
  def diff(that: Clause) = new Clause(ant diff that.ant, suc diff that.suc)
  def intersect(that: Clause) = new Clause(ant intersect that.ant, suc intersect that.suc)

  def first: Literal = literals.head
  def last: Literal = literals.last

  def map[R](antF: E => R, sucF: E => R): (Set[R], Set[R]) = (ant.map(antF), suc.map(sucF))
}

object Clause {
  def apply(left: E*)(right: E*)  = new Clause(ListSet(left: _*), ListSet(right: _*))
  def empty = new Clause(ListSet.empty, ListSet.empty)
}

