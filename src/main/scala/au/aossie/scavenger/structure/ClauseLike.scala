package au.aossie.scavenger.structure 


import au.aossie.scavenger.expression.E
import au.aossie.scavenger.util.unicode._
import au.aossie.scavenger.util.rich.RichIterable._
import language.reflectiveCalls
import language.implicitConversions

/** A trait for sequent-like data structures.
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
trait ClauseLike[+Repr <: ClauseLike[Repr]] {
  def ant: Iterable[E]
  def suc: Iterable[E]  

  def width = ant.size + suc.size
  def size = width + 1
  def logicalSize = ((ant ++ suc).map(_.logicalSize) :\ 0)(_ + _ + 1) 
 
  def isEmpty = ant.isEmpty && suc.isEmpty
  def isUnit: Boolean = { width == 1 }
  
  def antContains(f:E) = ant.contains(f)
  def sucContains(f:E) = suc.contains(f)
  def contains(f:E) = antContains(f) || sucContains(f)
  
  def subsequentOf(that: AbstractClause) = ant.forall(f => that.ant contains f) && suc.forall(f => that.suc contains f)
  
  def isTautological = ant.exists(f => suc contains f)
  
  def +(f:E): Repr
  def +:(f:E): Repr
  def -(f:E): Repr
  def -:(f:E): Repr
  
  def union(that: AbstractClause): Repr
  def diff(that: AbstractClause): Repr
  def intersect(that: AbstractClause): Repr
 
}
