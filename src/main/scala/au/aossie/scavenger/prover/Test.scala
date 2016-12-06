package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable.CNF

import scala.collection.mutable
import au.aossie.scavenger.expression.{ Sym, E, App, Abs }
import au.aossie.scavenger.parser.TPTPCNFParser

object Test {
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
    val p = TPTPCNFParser.parse("examples/problems/TPTP-v6.4.0/Problems/" +  testName.take(3) + "/" + testName + ".p")
    implicit val vars = getUppercaseVariables(p)
    CR.prove(p)
  }

  def main(args: Array[String]): Unit = {
    println(test("GRP135-2.002"))
  }
}
