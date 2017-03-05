package au.aossie.scavenger.parser.TPTP

import ammonite.ops._
import au.aossie.scavenger.expression.{E, Sym}
import au.aossie.scavenger.parser.TPTP.TPTPAST.{AnnotatedFormula, SimpleFormula, TPTPDirective}

import collection.mutable.Set
/**
  * Created by eze on 2016.05.25..
  */
object FOF extends FOF

/**
  * The ProblemParserFOFTPTP trait implements a parser for problems written
  * in the TPTP FOF syntax. We assume that there are no derivation nodes in
  * the parsed file, i.e. that we only have our axioms and conjectures.
  */
trait FOF
extends Base {

  def problemParser : Parser[FOFProblem] = TPTP_file ^^ generateProblem

  def generateProblem(directives : List[TPTPDirective]) : FOFProblem = {
    val formulas   : List[(String,String,E)]     = directives map extractFormulas
    val statements : List[FOFProblemStatement]   = formulas map ((t : (String,String,E)) => formulaToStatement(t._1,t._2,t._3))
    FOFProblem(statements,getSeenVars)
  }


  def extractFormulas(directive : TPTPDirective) : (String,String,E) =
    directive match {
      case AnnotatedFormula(language,name,role,SimpleFormula(formula),_) if language == lexical.FOF.chars => (name,role,formula)
      case _                                                                                              => throw new Exception("Unexpected Format")
    }

  def formulaToStatement(name : String, role : String, formula : E) : FOFProblemStatement =
    role match {
      case "conjecture"         => FOFConjectureStatement(name,formula)
      case "negated_conjecture" => FOFNegatedConjectureStatement(name,formula)
      case _                    => FOFAxiomStatement(name,formula)
    }


  def problem(fileName : Path) : FOFProblem = extract(fileName,problemParser)
}

class FOFProblem(val statements : List[FOFProblemStatement], val variables : Set[Sym]) {
  override def toString : String = statements.mkString("\n") + "\nVariables: " + variables.mkString(",")
}
object FOFProblem {
  def apply(statements: List[FOFProblemStatement],variables : Set[Sym]): FOFProblem = new FOFProblem(statements,variables)
}

abstract class FOFProblemStatement
case class FOFAxiomStatement(name : String, formula : E) extends FOFProblemStatement {
  override def toString : String = "fof(" + name + ",axiom," + formula.toString + ")"
}
case class FOFConjectureStatement(name : String, formula : E) extends FOFProblemStatement {
  override def toString : String = "fof(" + name + ",conjecture," + formula.toString + ")"
}
case class FOFNegatedConjectureStatement(name : String, formula : E) extends FOFProblemStatement {
  override def toString : String = "fof(" + name + ",negated_conjecture," + formula.toString + ")"
}
