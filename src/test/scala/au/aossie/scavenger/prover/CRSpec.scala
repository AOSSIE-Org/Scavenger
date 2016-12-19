package au.aossie.scavenger.prover

import au.aossie.scavenger.expression._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import au.aossie.scavenger.structure.immutable.{ CNF, SetClause => Clause }
import org.specs2.mutable.Specification

/**
  * @author Daniyar Itegulov
  */
class CRSpec extends Specification {
  val x = Sym("x")
  val y = Sym("y")
  val z = Sym("z")
  val a = Sym("a")
  val b = Sym("b")
  val d = Sym("d")
  val P = Sym("P")
  val f = Sym("f")
  val Pa = App(P, a)
  val Pb = App(P, b)
  val Pd = App(P, d)
  val Px = App(P, x)
  val Py = App(P, y)
  val Pz = App(P, z)
  implicit val vars = mutable.Set(x, y, z)

  private def test(clauses: Clause*) = CR.prove(CNF(ArrayBuffer(clauses: _*)))

  private def clause(ant: E*)(suc: E*) = new Clause(ant.toSet, suc.toSet)

  "CR" should {
    "find satisfiable" in {
      test(
        clause()(App(P, x)),
        clause()(App(P, a))
      ).isInstanceOf[Satisfiable] shouldEqual true
    }

    "find unsatisfiable" in {
      test(
        clause(App(P, a))(), // P(a)
        clause(App(P, App(f, x)))(App(P, x)), // ∀x.(P(x) or !P(f(x))
        clause()(App(P, App(f, App(f, a)))) // P(f(f(a)))
      ).isInstanceOf[Unsatisfiable] shouldEqual true

      test(
        clause()(Pa, Pb),
        clause(Pa)(Px),
        clause(Pb)(Py),
        clause(Pa, Pb)(),
        clause()(Pd),
        clause(Pz)(App(P, App(f, z)))
      ).isInstanceOf[Unsatisfiable] shouldEqual true
    }
  }
}

