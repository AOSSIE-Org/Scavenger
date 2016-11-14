package au.aossie.scavenger.structure 

import au.aossie.scavenger.util.unicode._


/** An abstract superclass for all kinds of clauses.
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
abstract class AbstractClause extends ClauseLike[AbstractClause] {

  def toSetSequent = new immutable.SetClause(ant.toSet, suc.toSet)
  def toSeqSequent = new immutable.SeqClause(ant.toSeq, suc.toSeq)
  
  override def equals(v: Any) = v match {    
      case that: AbstractClause => (that canEqual this) && (ant == that.ant) && (suc == that.suc) 
      case _ => false   
  }   
  def canEqual(other: Any) = other.isInstanceOf[AbstractClause]  
  override def hashCode = 42*ant.hashCode + suc.hashCode
  override def toString = ant.mkString(", ") + unicodeOrElse(" \u22A2 "," :- ") + suc.mkString(", ")
  
}