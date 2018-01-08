package org.aossie.scavenger.structure
package immutable

import org.aossie.scavenger.expression.{AppRec, E, Sym}
import org.aossie.scavenger.util.unicode.unicodeOrElse

import scala.collection.immutable.ListSet

sealed trait ClauseType
case object AxiomClause extends ClauseType
case object ConjectureClause extends ClauseType
case object NegConjectureClause extends ClauseType
case object HypothesisClause extends ClauseType
case object UndefClause extends ClauseType

/**
 *
 *  @author  Bruno Woltzenlogel Paleo
 *  @version 0.1
 *  @since   0.1
 */
class Clause(val ant: ListSet[E], val suc: ListSet[E], val tp: ClauseType = UndefClause) extends ClauseLike[Clause] {
  def literals: Seq[Literal] = ant.toSeq.map(Literal(_, polarity = false)) ++ suc.toSeq.map(Literal(_, polarity = true))
  def literal: Literal =
    if (ant.size == 1 && suc.isEmpty) Literal(ant.head, polarity = false)
    else if (ant.isEmpty && suc.size == 1) Literal(suc.head, polarity = true)
    else throw new IllegalStateException("Given SeqSequent is not a unit")

  def predicates: Seq[(Sym, Int)] = {
    (ant.map {
      case AppRec(fun: Sym, args) => (fun, args.size)
    } ++ suc.map {
      case AppRec(fun: Sym, args) => (fun, args.size)
    }).toSeq
  }
  def functionSymbols: Seq[(Sym, Int)] = {
    (ant.flatMap {
      case AppRec(_: Sym, args) => args.flatMap(_.functionSymbols)
    } ++ suc.flatMap {
      case AppRec(_: Sym, args) => args.flatMap(_.functionSymbols)
    }).toSeq
  }
  def constantSymbols: Seq[Sym] = {
    (ant.flatMap {
      case AppRec(_: Sym, args) => args.flatMap(_.constantSymbols)
    } ++ suc.flatMap {
      case AppRec(_: Sym, args) => args.flatMap(_.constantSymbols)
    }).toSeq
  }

  lazy val toTPTPString: String = {
    if (ant.isEmpty && suc.isEmpty) {
      "$false"
    } else {
      val splitter = {
        if (ant.isEmpty || suc.isEmpty) {
          ""
        } else {
          " | "
        }
      }
      ant.map(expr => s"~(${expr.toString})").mkString(" | ") + splitter + suc.mkString(" | ")
    }
  }

  def maxDepth: Int = (ant.map(_.depth) ++ suc.map(_.depth)).max

  def +(f:E) = new Clause(ant, suc + f, tp)
  def +:(f:E) = new Clause(ant + f, suc, tp)
  def -(f:E) =  new Clause(ant, suc - f, tp)
  def -:(f:E) = new Clause(ant - f, suc, tp)

  def union(that: Clause) = new Clause(ant union that.ant, suc union that.suc, tp)
  def diff(that: Clause) = new Clause(ant diff that.ant, suc diff that.suc, tp)
  def intersect(that: Clause) = new Clause(ant intersect that.ant, suc intersect that.suc, tp)

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
  def apply(tp: ClauseType)(left: E*)(right: E*)  = new Clause(ListSet(left: _*), ListSet(right: _*), tp)
  def apply(left: E*)(right: E*)  = new Clause(ListSet(left: _*), ListSet(right: _*))
  def empty = new Clause(ListSet.empty, ListSet.empty)
}

