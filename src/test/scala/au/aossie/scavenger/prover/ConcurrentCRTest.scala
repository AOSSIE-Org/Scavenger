package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.{Sym, i}
import au.aossie.scavenger.judgment.immutable.Clause
import au.aossie.scavenger.parser.TPTPParsers._

import scala.collection.mutable

/**
  * @author Daniyar Itegulovg
  */
object ConcurrentCRTest extends App {

  def problemToClauses(problem: CNFProblem): Seq[Clause] = {
    problem.statements.map {
      case axiom: CNFAxiomStatement => new Clause(axiom.ant, axiom.suc)
      case negConj: CNFNegatedConjectureStatement => new Clause(negConj.ant, negConj.suc)
    }
  }

  def time[A](a: => A): Long = {
    val now = System.nanoTime
    val result = a
    (System.nanoTime - now) / 1000
  }

  def test(testName: String)(implicit vars: mutable.Set[Sym]) {
    val clauses = problemToClauses(ProblemParserCNFTPTP.problem(s"examples/problems/CNF/$testName.cnfp"))
    val t = time(ConcurrentCR.prove(new CNF(clauses)))
    println(s"Computed $testName in $t microseconds")
  }

  {
    implicit val variables = mutable.Set(Sym("V_x", i), Sym("V_U", i), Sym("T", i), Sym("T_a", i))
    test("ANA013-2")
  }

  {
    implicit val variables = mutable.Set(Sym("X", i), Sym("Y", i), Sym("Z", i))
    test("ALG002-1")
  }

  {
    implicit val variables = mutable.Set(Sym("X1", i), Sym("X2", i), Sym("X3", i))
    test("KRS001-1")
  }

  {
    implicit val variables = mutable.Set(Sym("X", i), Sym("Y", i), Sym("Z", i),
      Sym("V", i), Sym("U", i), Sym("W", i), Sym("A", i), Sym("B", i),
      Sym("C", i), Sym("D", i))
    test("FLD010-3")
  }

  {
    implicit val variables = mutable.Set(Sym("A", i), Sym("B", i), Sym("C", i),
      Sym("D", i), Sym("E", i), Sym("F", i),
      Sym("G", i), Sym("H", i), Sym("I", i))
    test("MGT017-1")
  }

  {
    implicit val variables = mutable.Set(Sym("A", i), Sym("B", i),
      Sym("X", i), Sym("Y", i), Sym("Z", i))
    test("NUM019-1")
  }

  {
    implicit val variables = mutable.Set(Sym("A", i), Sym("B", i),
      Sym("X", i), Sym("Y", i), Sym("Z", i))
    test("PUZ008-2")
  }

  {
    implicit val variables = mutable.Set(Sym("Subset", i), Sym("Superset", i),
      Sym("Element", i), Sym("Set1", i), Sym("Set2", i), Sym("Intersection", i))
    test("SET006-1")
  }
}
