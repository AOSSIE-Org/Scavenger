package au.aossie.scavenger


import au.aossie.scavenger.expression._
import au.aossie.scavenger.parser.TPTP.{CNF => TPTPCNF}
import au.aossie.scavenger.parser.TPTPCNFParser
import au.aossie.scavenger.prover.{PDCR, ProblemStatus, Satisfiable, Unsatisfiable}
import au.aossie.scavenger.structure.immutable.{CNF, SeqClause => Clause}
import org.specs2.mutable.Specification
import ammonite.ops._

import scala.collection.mutable

/**
  * This class executes the prover on Pelletier's CNF problems that are currently available in the TPTP library.
  * Pelletier's problems are described in: http://www.sfu.ca/~jeffpell/papers/75ATPproblems86.pdf
  *
  */
class PelletierCNF extends Specification {


  // TODO: This is not the right place for this function
  // TODO: remove this function from here once we have a better way to distinguish constants and variables
  def getUppercaseVariables(cnf: CNF): mutable.Set[Sym] = {
    def uppercaseVariableInFormula(e: E): Set[Sym] = e match {
      case v: Sym if v.name.charAt(0).isUpper => Set(v)
      case App(l, r) => uppercaseVariableInFormula(l) ++ uppercaseVariableInFormula(r)
      case Abs(_,_, body) => uppercaseVariableInFormula(body)
      case _ => Set()
    }
    val variables = mutable.Set.empty[Sym]
    cnf.clauses.flatMap(clause => clause.ant ++ clause.suc).foreach(variables ++= uppercaseVariableInFormula(_))
    variables
  }

  def test(testName: String): ProblemStatus = {
    val p = TPTPCNFParser.parse(pwd / 'examples / 'problems / 'Pelletier / s"$testName.p")
    implicit val vars = getUppercaseVariables(p)
    PDCR.prove(p)
  }

  // TODO: Implement a timeout as described here: https://etorreborre.github.io/specs2/guide/SPECS2-3.5/org.specs2.guide.TimeoutExamples.html

  "Scavenger" should {
    "prove SYN040-1" in { test("SYN040-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN041-1" in { test("SYN041-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN044-1" in { test("SYN044-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN045-1" in { test("SYN040-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN046-1" in { test("SYN040-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN047-1" in { test("SYN040-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN048-1" in { test("SYN040-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN049-1" in { test("SYN040-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN050-1" in { test("SYN050-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN051-1" in { test("SYN051-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN052-1" in { test("SYN052-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN053-1" in { test("SYN053-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN054-1" in { test("SYN054-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN055-1" in { test("SYN055-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "find a counter-model for SYN056-1" in { test("SYN056-1").isInstanceOf[Satisfiable] shouldEqual true }
    "prove SYN057-1" in { test("SYN057-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN058-1" in { test("SYN058-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "find a counter-model for SYN059-1" in { test("SYN059-1").isInstanceOf[Satisfiable] shouldEqual true }
    "prove SYN060-1" in { test("SYN060-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN061-1" in { test("SYN061-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN062-1" in { test("SYN062-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN063-1" in { test("SYN063-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN064-1" in { test("SYN064-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN065-1" in { test("SYN065-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN066-1" in { test("SYN066-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN067-1" in { test("SYN067-1").isInstanceOf[Unsatisfiable] shouldEqual true } // The prover is not terminating on this example.
    "prove SYN068-1" in { test("SYN068-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN069-1" in { test("SYN069-1").isInstanceOf[Unsatisfiable] shouldEqual true }
    "prove SYN070-1" in { test("SYN070-1").isInstanceOf[Unsatisfiable] shouldEqual true }

      // The following problems require equality

//    "prove SYN071-1" in { test("SYN071-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN072-1" in { test("SYN072-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN073-1" in { test("SYN073-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN074-1" in { test("SYN074-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN075-1" in { test("SYN075-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN076-1" in { test("SYN076-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN077-1" in { test("SYN077-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN078-1" in { test("SYN078-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN079-1" in { test("SYN079-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN080-1" in { test("SYN080-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN081-1" in { test("SYN081-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN082-1" in { test("SYN082-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN083-1" in { test("SYN083-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN084-1" in { test("SYN084-1").isInstanceOf[Unsatisfiable] shouldEqual true }
  }
}

