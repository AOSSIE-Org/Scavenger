package au.aossie.scavenger.judgment 
package mutable

import au.aossie.scavenger.expression.E


/** A trait for mutable sequent-like data structures.
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
trait SequentLike[+This <: SequentLike[This]] extends au.aossie.scavenger.judgment.SequentLike[This] {
  /** Adds a formula to the succedent of the sequent.
   *  @param f the formula to be added.
   *  @return the sequent itself.
   *  @example `s += f`
   */
  def +=(f:E): This
  
  /** Adds a formula to the antecedent of the sequent.
   *  @param f the formula to be added.
   *  @return the sequent itself.
   *  @example `s += f`
   */
  def =+:(f:E): This
  
  /** Removes a formula from the succedent of the sequent.
   *  @param f the formula to be removed.
   *  @return the sequent itself.
   *  @example `s += f`
   */
  def -=(f:E): This
  
  /** Removes a formula from the antecedent of the sequent.
   *  @param f the formula to be removed.
   *  @return the sequent itself.
   *  @example `s += f`
   */
  def =-:(f:E): This
}