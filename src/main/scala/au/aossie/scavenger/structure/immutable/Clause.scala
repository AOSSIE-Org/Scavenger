package au.aossie.scavenger.structure
package immutable

import language.implicitConversions

import au.aossie.scavenger.expression.E
import collection.mutable.Stack
import au.aossie.scavenger.expression.formula.Neg
  

/** A class for immutable sequents whose cedents are seqs.
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
class Clause(val ant: Seq[E], val suc: Seq[E]) extends Sequent with ClauseLike[Clause] { 
  def +(f:E) = new Clause(ant, suc :+ f)
  def +:(f:E) = new Clause(ant :+ f, suc)
  def -(f:E) =  new Clause(ant, suc.filterNot(_ == f))
  def -:(f:E) = new Clause(ant.filterNot(_ == f), suc)
 
  def union(that: Sequent) = new Clause(ant union that.ant.toSeq, suc union that.suc.toSeq)
  def diff(that: Sequent) = new Clause(ant diff that.ant.toSeq, suc diff that.suc.toSeq)
  def intersect(that: Sequent) = new Clause(ant intersect that.ant.toSeq, suc intersect that.suc.toSeq)  
  
  // ToDo: Think about what to do with these methods
  def -*(f:E) = new Clause(ant, suc.filterNot(_ eq f)) 
  def -*:(f:E) = new Clause(ant.filterNot(_ eq f), suc)  
  def --*(s:Clause) = new Clause(ant.filterNot(f => s.ant.exists(_ eq f)), suc.filterNot(f => s.suc.exists(_ eq f)))
  
  def literals: Seq[Literal] =
    ant.map(Literal(_, negated = true)) ++ suc.map(Literal(_, negated = false))

  // TODO: this is inefficient
  def apply(pos: Int): Literal = literals(pos)

  def first: Literal = apply(0)

  def last: Literal = apply(literals.length - 1)

  def isUnit: Boolean = { width == 1 }
}

object Clause {
  def apply(ant:E*)(suc:E*) = new Clause(ant, suc)
  
  def empty = Clause()()
 
  implicit def fromTraversable(s: TraversableOnce[E]) = {
    val ant = new Stack[E]; val suc = new Stack[E];
    for (f <- s) f match {
      case Neg(g) => ant.push(g)
      case _ => suc.push(f)
    }
    new Clause(ant,suc)
  } 
}

