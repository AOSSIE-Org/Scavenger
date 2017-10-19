package org.aossie.scavenger.parser

import ammonite.ops._
import org.aossie.scavenger.parser.TPTP.{CNFAxiomStatement, CNFHypothesisStatement, CNFNegatedConjectureStatement, CNF => TPTPCNF}
import org.aossie.scavenger.structure.immutable._


/**
  * @author Daniyar Itegulov
  */
class TPTPCNFParser(val includesDir: Path) extends Parser with TPTPCNF {
  def parse(filepath: Path): CNF = {
    val p = problem(filepath)
    val clauses = p.statements.map {
      case axiom: CNFAxiomStatement => Clause(AxiomClause)(axiom.ant: _*)(axiom.suc: _*)
      case negConj: CNFNegatedConjectureStatement => Clause(NegConjectureClause)(negConj.ant: _*)(negConj.suc: _*)
      case hyp: CNFHypothesisStatement => Clause(HypothesisClause)(hyp.ant: _*)(hyp.suc: _*)
      //TODO: the case of CNFConjectureStatement seems to be missing
    }
    CNF(clauses)
  }
}
