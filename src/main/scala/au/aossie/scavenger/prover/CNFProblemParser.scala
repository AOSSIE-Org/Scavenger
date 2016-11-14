package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable.SeqClause
import au.aossie.scavenger.parser.TPTP.{CNFAxiomStatement, CNFNegatedConjectureStatement, ProblemParserCNFTPTP}

import au.aossie.scavenger.structure.immutable.CNF

/**
  * @author Daniyar Itegulov
  */
object CNFProblemParser extends ProblemParser {
  override def parse(filename: String): CNF = {
    val problem = ProblemParserCNFTPTP.problem(filename)
    val clauses = problem.statements.map {
      case axiom: CNFAxiomStatement => new SeqClause(axiom.ant, axiom.suc)
      case negConj: CNFNegatedConjectureStatement => new SeqClause(negConj.ant, negConj.suc)
    }
    new CNF(clauses)
  }
}
