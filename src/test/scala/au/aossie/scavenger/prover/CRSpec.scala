package au.aossie.scavenger.prover

import au.aossie.scavenger.expression._
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
@RunWith(classOf[JUnitRunner])
class CRSpec extends SpecificationWithJUnit {
  val x = Sym("x", i)
  val y = Sym("y", i)
  val z = Sym("z", i)
  val a = Sym("a", i)
  val b = Sym("b", i)
  val d = Sym("d", i)
  val P = Sym("P", i -> o)
  val f = Sym("f", i -> i)
  val Pa = App(P, a)
  val Pb = App(P, b)
  val Pd = App(P, d)
  val Px = App(P, x)
  val Py = App(P, y)
  val Pz = App(P, z)
  implicit val vars = mutable.Set(x, y, z)

  private def test(clauses: Clause*) = CR.prove(new CNF(ArrayBuffer(clauses:_*)))

  "CR" should {
    "find satisfiable" in {
      test(
        Clause()(App(P, x)),
        Clause()(App(P, a))
      ) shouldEqual Some
    }

    "find unsatisfiable" in {
      test(
        Clause(App(P, a))(), // P(a)
        Clause(App(P, App(f, x)))(App(P, x)), // ∀x.(P(x) or !P(f(x))
        Clause()(App(P, App(f, App(f, a)))) // P(f(f(a)))
      ) shouldEqual Some

      test(
        Clause()(Pa, Pb),
        Clause(Pa)(Px),
        Clause(Pb)(Py),
        Clause(Pa, Pb)(),
        Clause()(Pd),
        Clause(Pz)(App(P, App(f, z)))
      ) shouldEqual Some
    }
  }
}
