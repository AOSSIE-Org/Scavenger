package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable._
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

  private def test(clauses: SetClause*) = CDCL.isSatisfiable(CNF(ArrayBuffer(clauses: _*)))

  "CDCL" should {
    "find satisfiable" in {
      test(
        SetClause(a, b)(),
        SetClause()(b, c),
        SetClause(c)(d),
        SetClause()(a)
      ) shouldEqual true

      test(
        SetClause(a)(b),
        SetClause(b, c)(),
        SetClause(d)(c)
      ) shouldEqual true

      test(
        SetClause(b)(a, x),
        SetClause(c)(a),
        SetClause()(b, c, d)
      ) shouldEqual true

      test(
        SetClause(b)(a, x),
        SetClause(c)(a),
        SetClause()(b, c, d),
        SetClause(d, e)(),
        SetClause(d, f)(y),
        SetClause()(e, f)
      ) shouldEqual true
    }

    "find unsatisfiable" in {
      test(
        SetClause(a)(),
        SetClause()(a)
      ) shouldEqual false
    }
  }
}

