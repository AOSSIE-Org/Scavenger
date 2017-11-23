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
  val c = Sym("c")
  val d = Sym("d")
  val e = Sym("e")
  val f = Sym("f")
  val g = Sym("g")
  val h = Sym("h")
  val P = Sym("P")
  val Q = Sym("Q")
  val R = Sym("R")
  val S = Sym("S")
  val Pa = App(P, a)
  val Pb = App(P, b)
  val Pd = App(P, d)
  val Px = App(P, x)
  val Py = App(P, y)
  val Pz = App(P, z)

  private def test(clauses: Clause*) = TDCR.prove(CNF(ArrayBuffer(clauses: _*)))

  private def clause(ant: E*)(suc: E*) = Clause(ant.toSeq: _*)(suc.toSeq: _*)

  "CR" should {
    "find unsatisfiable" in {
      test(
        clause()(App(S, x), App(P, x), App(Q, x), App(R, x)),
        clause(App(R, x))(App(S, x), App(P, x), App(Q, x)),
        clause(App(Q, x))(App(S, x), App(P, x), App(R, x)),
        clause(App(Q, x), App(R, x))(App(S, x), App(P, x)),
        clause(App(P, x))(App(S, x), App(Q, x), App(R, x)),
        clause(App(P, x), App(R, x))(App(S, x), App(Q, x)),
        clause(App(P, x), App(Q, x))(App(S, x), App(R, x)),
        clause(App(P, x), App(Q, x), App(R, x))(App(S, x)),
        clause(App(S, x))(App(P, x), App(Q, x), App(R, x)),
        clause(App(S, x), App(R, x))(App(P, x), App(Q, x)),
        clause(App(S, x), App(Q, x))(App(P, x), App(R, x)),
        clause(App(S, x), App(Q, x), App(R, x))(App(P, x)),
        clause(App(S, x), App(P, x))(App(Q, x), App(R, x)),
        clause(App(S, x), App(P, x), App(R, x))(App(Q, x)),
        clause(App(S, x), App(P, x), App(Q, x))(App(R, x)),
        clause(App(S, x), App(P, x), App(Q, x), App(R, x))(),
        clause()(App(P, a)),
        clause()(App(Q, b)),
        clause()(App(R, c)),
        clause()(App(S, d)),
        clause(App(P, e))(),
        clause(App(Q, f))(),
        clause(App(R, g))(),
        clause(App(S, h))(),
      ).isInstanceOf[Unsatisfiable] shouldEqual true
    }
  }
}

