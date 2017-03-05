package au.aossie.scavenger.parser
import ammonite.ops.Path
import au.aossie.scavenger.structure.immutable.{CNF, Clause}
import au.aossie.scavenger.parser.TPTP.{FOFAxiomStatement, FOFConjectureStatement, FOFNegatedConjectureStatement, FOF => TPTPFOF}

/**
  * @author Vlad Podtelkin
  */
object TPTPFOFParser {
  def parse(filename: Path): CNF = {
    val problem = TPTPFOF.problem(filename)
    val clauses = problem.statements.map {
      case axiom: FOFAxiomStatement => {
        TPTPClausifier.clausify(axiom.formula)
      }
      case neg_conj: FOFNegatedConjectureStatement => {
        TPTPClausifier.clausify(neg_conj.formula)
      }
      case conj: FOFConjectureStatement => {
        TPTPClausifier.clausify(conj.formula)
      }
    }.fold(CNF(Seq[Clause]()))(_ + _)
    clauses
  }
}
