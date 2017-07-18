package au.aossie.scavenger.parser
import ammonite.ops.Path
import au.aossie.scavenger.expression.formula.{And, Neg}
import au.aossie.scavenger.structure.immutable.{AxiomClause, CNF, Clause, NegConjectureClause}
import au.aossie.scavenger.parser.TPTP.{FOFAxiomStatement, FOFConjectureStatement, FOFNegatedConjectureStatement, FOF => TPTPFOF}
import au.aossie.scavenger.preprocessing.TPTPClausifier

/**
  * @author Vlad Podtelkin
  */
object TPTPFOFParser extends Parser {
  def parse(filename: Path): CNF = {
    val problem = TPTPFOF.problem(filename)
    val formulas = problem.statements.map {
      case axiom: FOFAxiomStatement => {
        (axiom.formula, AxiomClause)
      }
      case neg_conj: FOFNegatedConjectureStatement => {
        (neg_conj.formula, NegConjectureClause)
      }
      case conj: FOFConjectureStatement => {
        (Neg(conj.formula), NegConjectureClause)
      }
    }
    val res = new TPTPClausifier().apply(formulas)
//    res.clauses.foreach(clause => println(s"$clause :: ${clause.tp}"))
    res
  }
}
