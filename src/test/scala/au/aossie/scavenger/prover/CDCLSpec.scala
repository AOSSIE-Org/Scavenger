package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.{Sym, i}
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
@RunWith(classOf[JUnitRunner])
class CDCLSpec extends SpecificationWithJUnit {
  private val a = new Sym("A", i)
  private val b = new Sym("B", i)
  private val c = new Sym("C", i)
  private val d = new Sym("D", i)
  private val e = new Sym("E", i)
  private val f = new Sym("F", i)
  private val x = new Sym("X", i)
  private val y = new Sym("Y", i)
  private val z = new Sym("Z", i)

  private def test(clauses: Clause*) = CDCL.isSatisfiable(new CNF(ArrayBuffer(clauses:_*)))

  "CDCL" should {
    "find satisfiable" in {
      test(
        Clause(a, b)(),
        Clause()(b, c),
        Clause(c)(d),
        Clause()(a)
      ) shouldEqual true

      test(
        Clause(a)(b),
        Clause(b, c)(),
        Clause(d)(c)
      ) shouldEqual true

      test(
        Clause(b)(a, x),
        Clause(c)(a),
        Clause()(b, c, d)
      ) shouldEqual true

      test(
        Clause(b)(a, x),
        Clause(c)(a),
        Clause()(b, c, d),
        Clause(d, e)(),
        Clause(d, f)(y),
        Clause()(e, f)
      ) shouldEqual true
    }

    "find unsatisfiable" in {
      test(
        Clause(a)(),
        Clause()(a)
      ) shouldEqual false
    }
  }
}
