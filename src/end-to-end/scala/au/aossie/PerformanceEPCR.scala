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
    val cat = testName.take(3)
    val p = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / s"$cat" / s"$testName.p")
    implicit val vars = getUppercaseVariables(p)
    EPCR.prove(p)
  }

  // TODO: Implement a timeout as described here: https://etorreborre.github.io/specs2/guide/SPECS2-3.5/org.specs2.guide.TimeoutExamples.html

//  "Scavenger" should {
//    "prove MSC004-1" in { test("MSC004-1").isInstanceOf[Unsatisfiable] shouldEqual true } //
//    "prove SYN352-1" in { test("SYN352-1").isInstanceOf[Unsatisfiable] shouldEqual true } //
//    "prove SET011-1" in { test("SET011-1").isInstanceOf[Unsatisfiable] shouldEqual true } //
//    "prove COM011-2" in { test("COM011-2").isInstanceOf[Unsatisfiable] shouldEqual true } //
//    "prove NLP001-1" in { test("NLP001-1").isInstanceOf[Unsatisfiable] shouldEqual true } //
////    "prove SET007-1" in { test("SET007-1").isInstanceOf[Unsatisfiable] shouldEqual true } // works in scavenger-20, TL now
////    "prove SWV321-2" in { test("SWV321-2").isInstanceOf[Unsatisfiable] shouldEqual true } //
//
//
//  }
}
