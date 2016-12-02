package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.structure.immutable.{CNF, SeqClause => Clause}
import au.aossie.scavenger.parser.TPTP.{CNFAxiomStatement, CNFNegatedConjectureStatement, CNFProblem, CNF => TPTPCNF}

import scala.collection.mutable

/**
  * Created by itegulov on 22.07.16.
  */
object CRTest extends App {

  def problemToClauses(problem: CNFProblem): Seq[Clause] = {
    problem.statements.map {
      case axiom: CNFAxiomStatement => Clause(axiom.ant, axiom.suc)
      case negConj: CNFNegatedConjectureStatement => Clause(negConj.ant, negConj.suc)
    }
  }

  def time[A](a: => A): Long = {
    val now = System.nanoTime
    val result = a
    (System.nanoTime - now) / 1000
  }

  def test(testName: String)(implicit vars: mutable.Set[Sym]) {
    val clauses = problemToClauses(TPTPCNF.problem(s"examples/problems/CNF/$testName.cnfp"))
    val t = time(CR.prove(new CNF(clauses)))
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

  // Bloats too fast -- need some time to finish
//  {
//    val clauses = problemToClauses(ProblemParserCNFTPTP.problem("examples/problems/CNF/LCL031-1.cnfp"))
//    implicit val variables = mutable.Set(Var("X"), Var("Y"), Var("Z"))
//    println(CR.prove(new CNF(clauses)))
//  }

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

  {
    implicit val variables = mutable.Set(Sym("X"), Sym("Y"), Sym("Z"))
    test("ANA002-4")
  }

  {
    implicit val variables = mutable.Set(Sym("X"), Sym("Y"), Sym("Z"),
      Sym("V"), Sym("U"), Sym("W"), Sym("A"), Sym("B"),
      Sym("C"), Sym("D"))
    test("FLD041-3")
  }
}

