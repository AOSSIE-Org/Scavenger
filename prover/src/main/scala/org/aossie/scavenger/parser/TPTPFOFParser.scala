package org.aossie.scavenger.parser

import ammonite.ops.Path
import org.aossie.scavenger.expression.formula.Neg
import org.aossie.scavenger.structure.immutable.{AxiomClause, CNF, NegConjectureClause}
import org.aossie.scavenger.parser.TPTP.{FOFAxiomStatement, FOFConjectureStatement, FOFNegatedConjectureStatement, FOF => TPTPFOF}
import org.aossie.scavenger.preprocessing.TPTPClausifier

/**
  * @author Vlad Podtelkin
  */
class TPTPFOFParser(val includesDir: Path) extends Parser with TPTPFOF {
  def parse(filename: Path): CNF = {
    val p = problem(filename)
    val formulas = p.statements.map {
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
    res
  }
}
