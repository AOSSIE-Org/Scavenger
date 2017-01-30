package au.aossie.scavenger.structure

import au.aossie.scavenger.util.unicode._

import au.aossie.scavenger.structure.immutable.Literal

/** An abstract superclass for all kinds of clauses.
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.2
 *  @since   0.2
 */
abstract class AbstractClause extends ClauseLike[AbstractClause] {

  def toSetSequent = immutable.SetClause(ant.toSeq: _*)(suc.toSeq: _*)
  def toSeqSequent = immutable.SeqClause(ant.toSeq, suc.toSeq)

  def literals = ant.toSeq.map(Literal(_, negated = true)) ++ suc.toSeq.map(Literal(_, negated = false))

  // TODO: the following three methods are inefficient.
  // I suspect they will not be used anymore when we use SetClause instead of SeqClause
  // When this is the case, we should simply remove these methods

  def apply(pos: Int): Literal = literals(pos)
  def first: Literal = apply(0)
  def last: Literal = apply(literals.length - 1)

  override def equals(v: Any) = v match {
      case that: AbstractClause => (that canEqual this) && (ant == that.ant) && (suc == that.suc)
      case _ => false
  }
  def canEqual(other: Any) = other.isInstanceOf[AbstractClause]
  override def hashCode = 42*ant.hashCode + suc.hashCode
  override def toString = ant.mkString(", ") + unicodeOrElse(" \u22A2 "," :- ") + suc.mkString(", ")

}
