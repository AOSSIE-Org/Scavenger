package au.aossie.scavenger.structure

import au.aossie.scavenger.expression.E
import au.aossie.scavenger.structure.immutable.Clause

import scala.language.{implicitConversions, reflectiveCalls}

/** A trait for sequent-like data structures.
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.1
 *  @since   0.1
 */
trait ClauseLike[+Repr <: ClauseLike[Repr]] {
  def ant: Set[E]
  def suc: Set[E]

  def width: Int = ant.size + suc.size
  def size: Int = width + 1
  def logicalSize: Int = ((ant ++ suc).map(_.logicalSize) :\ 0)(_ + _ + 1)

  def isEmpty: Boolean = ant.isEmpty && suc.isEmpty
  def isUnit: Boolean = { width == 1 }

  def antContains(f:E): Boolean = ant.contains(f)
  def sucContains(f:E): Boolean = suc.contains(f)
  def contains(f:E): Boolean = antContains(f) || sucContains(f)

  def subsequentOf(that: Clause): Boolean = ant.forall(f => that.ant contains f) && suc.forall(f => that.suc contains f)

  def isTautological: Boolean = ant.exists(f => suc contains f)

  def +(f:E): Repr
  def +:(f:E): Repr
  def -(f:E): Repr
  def -:(f:E): Repr

  def union(that: Clause): Repr
  def diff(that: Clause): Repr
  def intersect(that: Clause): Repr

}

