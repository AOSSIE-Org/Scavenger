package org.aossie.scavenger.preprocessing

import org.aossie.scavenger.expression.{AppRec, Sym}
import org.aossie.scavenger.structure.immutable.{Clause, ClauseType, Literal}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

/**
  * Created by vlad107 on 7/12/17.
  */
object ClausesTo3CNF {
  def to3CNF(clauses: ListBuffer[Clause])(implicit rnd: Random): Unit = {

    def genNewSym(usedSyms: mutable.HashSet[Sym]): Sym = {
      val c = (rnd.nextInt(26) + 'a'.toInt).toChar
      var i = 0
      while (usedSyms.contains(Sym(s"$c$i"))) i += 1
      val newSym = Sym(s"$c$i")
      usedSyms.add(newSym)
      newSym
    }

    def literalsToClause(literals: Literal*)(tp: ClauseType): Clause = {
      val (positiveLiterals, negativeLiterals) = literals.partition(_.polarity)
      Clause(tp)(negativeLiterals.map(_.unit): _*)(positiveLiterals.map(_.unit): _*)
    }

    val usedSyms = clauses.flatMap(clause =>
      clause.literals.flatMap(_.unit.functionSymbols.map(_._1))
    )(collection.breakOut).to[mutable.HashSet]
    val nClauses = ListBuffer.empty[Clause]
    for (clause <- clauses) {
      var lits = clause.literals.to[ArrayBuffer]
      while (lits.size > 3) {
        val nlits = ArrayBuffer.empty[Literal]
        for (idx <- 0 until lits.length / 2) {
          val tmpSym = genNewSym(usedSyms)
          val args = (lits(idx * 2).unit.variables ++ lits(idx * 2 + 1).unit.variables).distinct
          val fun = Literal(AppRec(tmpSym, args), true)
          nClauses += literalsToClause(!fun, lits(idx * 2), lits(idx * 2 + 1))(clause.tp)
          nClauses += literalsToClause(!lits(idx * 2), fun)(clause.tp)
          nClauses += literalsToClause(!lits(idx * 2 + 1), fun)(clause.tp)
          nlits += fun
        }
        if (lits.length % 2 == 1) nlits += lits.last
        lits = nlits
      }
      nClauses += literalsToClause(lits: _*)(clause.tp)
    }
    clauses.clear
    clauses ++= nClauses
  }
}
