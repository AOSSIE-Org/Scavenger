package au.aossie

import ammonite.ops.pwd
import au.aossie.scavenger.expression.{Abs, App, E, Sym}
import au.aossie.scavenger.parser.TPTPCNFParser
import au.aossie.scavenger.prover.{EPCR, ProblemStatus, Satisfiable, Unsatisfiable}
import au.aossie.scavenger.structure.immutable.CNF
import org.specs2.mutable.Specification

import scala.collection.mutable

/**
  * Created by vlad107 on 6/9/17.
  */
class PerformanceEPCR extends Specification {
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
    val p = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'SYN / s"$testName.p")
    implicit val vars = getUppercaseVariables(p)
    EPCR.prove(p)
  }

  // TODO: Implement a timeout as described here: https://etorreborre.github.io/specs2/guide/SPECS2-3.5/org.specs2.guide.TimeoutExamples.html

  "Scavenger" should {
//    "find a counter-model for SYN056-1" in { test("SYN056-1").isInstanceOf[Satisfiable] shouldEqual true }
//    "prove SYN143-1" in { test("SYN143-1").isInstanceOf[Unsatisfiable] shouldEqual true }
//    "prove SYN708-1" in { test("SYN708-1").isInstanceOf[Unsatisfiable] shouldEqual true } //
    "prove SYN645-1" in { test("SYN645-1").isInstanceOf[Unsatisfiable] shouldEqual true }
  }
}
