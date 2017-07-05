package au.aossie.scavenger.structure
package immutable

import au.aossie.scavenger.expression.E
import au.aossie.scavenger.util.unicode.unicodeOrElse

import scala.collection.immutable.ListSet

/**
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.1
 *  @since   0.1
 */
class Clause(val ant: ListSet[E], val suc: ListSet[E]) extends ClauseLike[Clause] {
  def literals: Seq[Literal] = ant.toSeq.map(Literal(_, polarity = false)) ++ suc.toSeq.map(Literal(_, polarity = true))

  def +(f:E) = new Clause(ant, suc + f)
  def +:(f:E) = new Clause(ant + f, suc)
  def -(f:E) =  new Clause(ant, suc - f)
  def -:(f:E) = new Clause(ant - f, suc)

  def union(that: Clause) = new Clause(ant union that.ant, suc union that.suc)
  def diff(that: Clause) = new Clause(ant diff that.ant, suc diff that.suc)
  def intersect(that: Clause) = new Clause(ant intersect that.ant, suc intersect that.suc)

  def first: Literal = literals.head
  def last: Literal = literals.last

  override def equals(v: Any): Boolean = v match {
    case that: Clause => (that canEqual this) && (ant == that.ant) && (suc == that.suc)
    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Clause]

  override def hashCode: Int = 42*ant.hashCode + suc.hashCode
  override def toString: String = ant.mkString(", ") + unicodeOrElse(" \u22A2 "," :- ") + suc.mkString(", ")

  def map[R](antF: E => R, sucF: E => R): (Set[R], Set[R]) = (ant.map(antF), suc.map(sucF))
}

object Clause {
  def apply(left: E*)(right: E*)  = new Clause(ListSet(left: _*), ListSet(right: _*))
  def empty = new Clause(ListSet.empty, ListSet.empty)
}

