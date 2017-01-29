package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable.CNF

import scala.collection.mutable
import au.aossie.scavenger.expression.{ Sym, E, App, Abs }
import au.aossie.scavenger.parser.TPTPCNFParser
import ammonite.ops._

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
    val p = TPTPCNFParser.parse(pwd / 'examples / 'problems / "TPTP-v6.4.0" / 'Problems / testName.take(3) / (testName + ".p"))
    implicit val vars = getUppercaseVariables(p)
    CR.prove(p)
  }

  def time[A](a: => A): Long = {
    val now    = System.nanoTime
    val result = a
    (System.nanoTime - now) / 1000
  }

  def main(args: Array[String]): Unit = {
    val executionTime = time(println(test("ALG001-1")))
    println(executionTime / 1000000.0)
  }
}
