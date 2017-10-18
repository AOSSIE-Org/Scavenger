package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable.{ CNF, Clause }
import au.aossie.scavenger.expression._
import org.specs2.mutable.Specification

import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
class CDCLSpec extends Specification {
  private val a = new Sym("A")
  private val b = new Sym("B")
  private val c = new Sym("C")
  private val d = new Sym("D")
  private val e = new Sym("E")
  private val f = new Sym("F")
  private val x = new Sym("X")
  private val y = new Sym("Y")
  private val z = new Sym("Z")

  private def test(clauses: Clause*) = CDCL.isSatisfiable(CNF(ArrayBuffer(clauses: _*)))

  private def clause(ant: E*)(suc: E*) = Clause(ant.toSeq: _*)(suc.toSeq: _*)

  "CDCL" should {
    "find satisfiable" in {
      test(
        clause(a, b)(),
        clause()(b, c),
        clause(c)(d),
        clause()(a)
      ) shouldEqual true

      test(
        clause(a)(b),
        clause(b, c)(),
        clause(d)(c)
      ) shouldEqual true

      test(
        clause(b)(a, x),
        clause(c)(a),
        clause()(b, c, d)
      ) shouldEqual true

      test(
        clause(b)(a, x),
        clause(c)(a),
        clause()(b, c, d),
        clause(d, e)(),
        clause(d, f)(y),
        clause()(e, f)
      ) shouldEqual true
    }

    "find unsatisfiable" in {
      test(
        clause(a)(),
        clause()(a)
      ) shouldEqual false
    }
  }
}
