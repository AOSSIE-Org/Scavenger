package au.aossie.scavenger.unification

import au.aossie.scavenger.expression.{AppRec, Sym}
import au.aossie.scavenger.structure.immutable.Literal
import au.aossie.scavenger.unification.{MartelliMontanari => unify}
import au.aossie.scavenger.unification.tools._

import scala.collection.mutable

/**
  * Created by podtelkin on 05.08.17.
  */
class Unificator {
  private val indexInListA: mutable.HashMap[Literal, Int] = mutable.HashMap.empty
  private val indexInListB: mutable.HashMap[Literal, Int] = mutable.HashMap.empty

  private val listA: mutable.ListBuffer[Literal] = mutable.ListBuffer.empty
  private val listB: mutable.ListBuffer[Literal] = mutable.ListBuffer.empty

  private val byPredicateA: mutable.HashMap[Sym, mutable.ListBuffer[Int]] = mutable.HashMap.empty
  private val byPredicateB: mutable.HashMap[Sym, mutable.ListBuffer[Int]] = mutable.HashMap.empty

  private val unifications: mutable.ListBuffer[mutable.ListBuffer[Int]] = mutable.ListBuffer.empty

  def addA(literal: Literal): Unit = {
    if (!indexInListA.contains(literal)) {
      listA += literal
      indexInListA.put(literal, listA.size - 1)
      unifications += mutable.ListBuffer.empty
      literal.unit match {
        case AppRec(predicate: Sym, _) =>
          byPredicateA.getOrElseUpdate(predicate, mutable.ListBuffer.empty).append(listA.size - 1)

          for (index <- byPredicateB.getOrElse(predicate, mutable.ListBuffer.empty)) if (literal.polarity != listB(index).polarity) {
            unifyWithRename(Seq(literal.unit), Seq(listB(index).unit)) match {
              case Some(_) =>
                unifications.last.append(index)
              case None =>
            }
          }
      }

    }
  }

  def addB(literal: Literal): Unit = {
    if (!indexInListB.contains(literal)) {
      listB += literal
      indexInListB.put(literal, listB.size - 1)
      literal.unit match {
        case AppRec(predicate: Sym, _) =>
          byPredicateB.getOrElseUpdate(predicate, mutable.ListBuffer.empty).append(listB.size - 1)

          for (index <- byPredicateA.getOrElse(predicate, mutable.ListBuffer.empty)) if (literal.polarity != listA(index).polarity) {
            unifyWithRename(Seq(literal.unit), Seq(listA(index).unit)) match {
              case Some(_) =>
                unifications(index).append(listB.size - 1)
              case None =>
            }
          }
      }
    }
  }

  def getUnifications(literal: Literal): mutable.ListBuffer[Literal] = indexInListA.get(literal) match {
    case Some(index) =>
      unifications(index).map(listB(_))
    case None =>
      mutable.ListBuffer.empty
  }
}
