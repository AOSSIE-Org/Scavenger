package au.aossie.scavenger.parser

import au.aossie.scavenger.structure.immutable.SeqClause
import au.aossie.scavenger.structure.immutable.CNF

import au.aossie.scavenger.parser.TPTP.{CNFAxiomStatement, CNFNegatedConjectureStatement, CNF => TPTPCNF}


/**
  * @author Daniyar Itegulov
  */
object TPTPCNFParser extends Parser {
  def parse(filename: String): CNF = {
    val problem = TPTPCNF.problem(filename)
    val clauses = problem.statements.map {
      case axiom: CNFAxiomStatement => new SeqClause(axiom.ant, axiom.suc)
      case negConj: CNFNegatedConjectureStatement => new SeqClause(negConj.ant, negConj.suc)
      //TODO: the case of CNFConjectureStatement seems to be missing
    }
    new CNF(clauses)
  }
}
