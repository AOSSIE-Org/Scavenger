package au.aossie.scavenger.structure
package immutable

import au.aossie.scavenger.expression.E
  
/** A class for immutable sequents whose cedents are immutable sets.
 *
 *  @example {{{
 *  // Make an empty SetSequent via the companion object factory
 *  val s = SetSequent()()
 *  
 *  // Add formula f to the succedent of sequent s
 *  val s1 = s + f
 *  
 *  // Add formula f to the antecedent of sequent s
 *  val s2 = f +: s
 *  
 *  // Compute the union of two sequents
 *  val s3 = s1 union s2
 *  
 *  }}}
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
class SetSequent(val ant: Set[E], val suc: Set[E]) extends Sequent with ClauseLike[SetSequent] { 
  def +(f:E) = new SetSequent(ant, suc + f)
  def +:(f:E) = new SetSequent(ant + f, suc)
  def -(f:E) =  new SetSequent(ant, suc - f)
  def -:(f:E) = new SetSequent(ant - f, suc)

  def union(that: Sequent) = new SetSequent(ant union that.ant.toSet, suc union that.suc.toSet)
  def diff(that: Sequent) = new SetSequent(ant diff that.ant.toSet, suc diff that.suc.toSet)
  def intersect(that:Sequent) = new SetSequent(ant intersect that.ant.toSet, suc intersect that.suc.toSet)
}

object SetSequent {
  def apply()()  = new SetSequent(Set(),Set())
  def empty = new SetSequent(Set(), Set())
}

