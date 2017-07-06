package au.aossie.scavenger.parser
import ammonite.ops.Path
import au.aossie.scavenger.expression.formula.And
import au.aossie.scavenger.structure.immutable.{CNF, Clause}
import au.aossie.scavenger.parser.TPTP.{FOFAxiomStatement, FOFConjectureStatement, FOFNegatedConjectureStatement, FOF => TPTPFOF}

/**
  * @author Vlad Podtelkin
  */
object TPTPFOFParser {
  def parse(filename: Path, _dependenciesDir: Option[Path] = None): CNF = {
    val problem = TPTPFOF.problem(filename, _dependenciesDir)
    val formula = problem.statements.map {
      case axiom: FOFAxiomStatement => {
        axiom.formula
      }
      case neg_conj: FOFNegatedConjectureStatement => {
        neg_conj.formula
      }
      case conj: FOFConjectureStatement => {
        conj.formula
      }
    }.reduce(And(_, _))
    TPTPClausifier(formula)
  }
}
