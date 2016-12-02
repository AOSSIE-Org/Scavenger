package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.structure.immutable.{ SeqClause => Clause }
import au.aossie.scavenger.parser._


import au.aossie.scavenger.structure.immutable._

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
object ConcurrentCRTest extends App {

  def problemToClauses(problem: TPTP.CNFProblem): Seq[Clause] = {
    problem.statements.map {
      case axiom: TPTP.CNFAxiomStatement => new Clause(axiom.ant, axiom.suc)
      case negConj: TPTP.CNFNegatedConjectureStatement => new Clause(negConj.ant, negConj.suc)
    }
  }

  def time[A](a: => A): Long = {
    val now = System.nanoTime
    val result = a
    (System.nanoTime - now) / 1000
  }

  def test(testName: String)(implicit vars: mutable.Set[Sym]) {
    val clauses = problemToClauses(TPTP.CNF.problem(s"examples/problems/CNF/$testName.cnfp"))
    val t = time(ConcurrentCR.prove(new CNF(clauses)))
    println(s"Computed $testName in $t microseconds")
  }

  {
    implicit val variables = mutable.Set(Sym("V_x"), Sym("V_U"), Sym("T"), Sym("T_a"))
    test("ANA013-2")
  }

  {
    implicit val variables = mutable.Set(Sym("X"), Sym("Y"), Sym("Z"))
    test("ALG002-1")
  }

  {
    implicit val variables = mutable.Set(Sym("X1"), Sym("X2"), Sym("X3"))
    test("KRS001-1")
  }

  {
    implicit val variables = mutable.Set(Sym("X"), Sym("Y"), Sym("Z"),
      Sym("V"), Sym("U"), Sym("W"), Sym("A"), Sym("B"),
      Sym("C"), Sym("D"))
    test("FLD010-3")
  }

  {
    implicit val variables = mutable.Set(Sym("A"), Sym("B"), Sym("C"),
      Sym("D"), Sym("E"), Sym("F"),
      Sym("G"), Sym("H"), Sym("I"))
    test("MGT017-1")
  }

  {
    implicit val variables = mutable.Set(Sym("A"), Sym("B"),
      Sym("X"), Sym("Y"), Sym("Z"))
    test("NUM019-1")
  }

  {
    implicit val variables = mutable.Set(Sym("A"), Sym("B"),
      Sym("X"), Sym("Y"), Sym("Z"))
    test("PUZ008-2")
  }

  {
    implicit val variables = mutable.Set(Sym("Subset"), Sym("Superset"),
      Sym("Element"), Sym("Set1"), Sym("Set2"), Sym("Intersection"))
    test("SET006-1")
  }
}

