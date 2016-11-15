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

  private def test(SeqClauses: SeqClause*) = CDCL.isSatisfiable(new CNF(ArrayBuffer(SeqClauses:_*)))

  "CDCL" should {
    "find satisfiable" in {
      test(
        SeqClause(a, b)(),
        SeqClause()(b, c),
        SeqClause(c)(d),
        SeqClause()(a)
      ) shouldEqual true

      test(
        SeqClause(a)(b),
        SeqClause(b, c)(),
        SeqClause(d)(c)
      ) shouldEqual true

      test(
        SeqClause(b)(a, x),
        SeqClause(c)(a),
        SeqClause()(b, c, d)
      ) shouldEqual true

      test(
        SeqClause(b)(a, x),
        SeqClause(c)(a),
        SeqClause()(b, c, d),
        SeqClause(d, e)(),
        SeqClause(d, f)(y),
        SeqClause()(e, f)
      ) shouldEqual true
    }

    "find unsatisfiable" in {
      test(
        SeqClause(a)(),
        SeqClause()(a)
      ) shouldEqual false
    }
  }
}
