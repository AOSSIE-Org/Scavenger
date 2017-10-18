package au.aossie.scavenger.prover.heuristic

import au.aossie.scavenger.unification.tools._
import au.aossie.scavenger.structure.immutable.{Clause, Literal}

import scala.collection.mutable

/**
  * Created by vlad107 on 18.07.17.
  */
class UnificationSearcher {

  val nonUnitClauses: mutable.Set[Clause] = mutable.HashSet.empty

  val literals: mutable.HashSet[Literal] = nonUnitClauses.flatMap(_.literals)(collection.breakOut).to[mutable.HashSet]

  val unifiableUnits: mutable.Map[Literal, mutable.Set[Literal]] = mutable.Map.empty

  def getUnifiers(literal: Literal): Seq[Literal] = unifiableUnits.getOrElse(literal, Set.empty).toSeq

  def clearUnifiableUnits(): Unit = unifiableUnits.clear()

  def removeNonValidLiterals(literals: Seq[Literal]): Unit = unifiableUnits.foreach {
    case (_, set) => set --= literals
  }

  def addNewClauses(newClauses: Seq[Clause]): Unit = {
    nonUnitClauses ++= newClauses
    literals ++= newClauses.flatMap(_.literals)(collection.breakOut)
  }

  def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {
    for (literal <- literals) {
      val set = unifiableUnits.getOrElseUpdate(literal, mutable.Set.empty)
      for (newLiteral <- newLiterals) {
        if (newLiteral.polarity != literal.polarity) {
          unifyWithRename(Seq(literal.unit), Seq(newLiteral.unit)) match {
            case Some(_) =>
              set += newLiteral
            case None =>
          }
        }
      }
    }
  }

  def clausesForPropagation(provedLiterals: mutable.Set[Literal]): mutable.Set[Clause] = {
    nonUnitClauses.filterNot(_.literals.exists(provedLiterals.contains))
  }

}
