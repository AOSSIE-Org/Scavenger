package org.aossie.scavenger.prover

import org.aossie.scavenger.expression._
import org.aossie.scavenger.structure.immutable.{CNF, Clause}
import org.specs2.mutable.Specification

import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
class CRSpec extends Specification {
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")
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

  private def test(clauses: Clause*) = PDCR.prove(CNF(ArrayBuffer(clauses: _*)))

  private def clause(ant: E*)(suc: E*) = Clause(ant.toSeq: _*)(suc.toSeq: _*)

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

