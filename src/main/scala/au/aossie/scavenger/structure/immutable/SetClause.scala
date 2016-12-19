package au.aossie.scavenger.structure
package immutable

import au.aossie.scavenger.expression.E

/**
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
class SetClause(val ant: Set[E], val suc: Set[E]) extends AbstractClause with ClauseLike[SetClause] {
  def +(f:E) = new SetClause(ant, suc + f)
  def +:(f:E) = new SetClause(ant + f, suc)
  def -(f:E) =  new SetClause(ant, suc - f)
  def -:(f:E) = new SetClause(ant - f, suc)

  def union(that: AbstractClause) = new SetClause(ant union that.ant.toSet, suc union that.suc.toSet)
  def diff(that: AbstractClause) = new SetClause(ant diff that.ant.toSet, suc diff that.suc.toSet)
  def intersect(that: AbstractClause) = new SetClause(ant intersect that.ant.toSet, suc intersect that.suc.toSet)

  def map[R](antF: E => R, sucF: E => R): (Set[R], Set[R]) = (ant.map(antF), suc.map(sucF))
}

object SetClause {
  def apply(left: E*)(right: E*)  = new SetClause(left.toSet, right.toSet)
  def empty = new SetClause(Set(), Set())
}

