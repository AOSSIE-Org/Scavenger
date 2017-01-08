package au.aossie.scavenger.parser

import ammonite.ops._
import au.aossie.scavenger.structure.immutable.{CNF, SeqClause, SetClause}
import au.aossie.scavenger.parser.TPTP.{CNFAxiomStatement, CNFNegatedConjectureStatement, CNF => TPTPCNF}


/**
  * @author Daniyar Itegulov
  */
object TPTPCNFParser extends Parser {
  def parse(filename: Path): CNF = {
    val problem = TPTPCNF.problem(filename)
    val clauses = problem.statements.map {
      case axiom: CNFAxiomStatement => SetClause(axiom.ant: _*)(axiom.suc: _*)
      case negConj: CNFNegatedConjectureStatement => SetClause(negConj.ant: _*)(negConj.suc: _*)
      //TODO: the case of CNFConjectureStatement seems to be missing
    }
    CNF(clauses)
  }
}
