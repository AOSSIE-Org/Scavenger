package au.aossie.scavenger.parser.TPTPParsers

import au.aossie.scavenger.expression.{E, Sym}
import au.aossie.scavenger.parser.BaseParserTPTP
import au.aossie.scavenger.parser.TPTPParsers.TPTPAST.{AnnotatedFormula, SimpleFormula, TPTPDirective}

import collection.mutable.Set

/**
  * Created by eze on 2016.07.05..
  */
object ProblemParserTFFTPTP extends ProblemParserTFFTPTP


/**
  * The ProblemParserTFFTPTP trait implements a parser for problems written
  * in the TPTP TFF syntax. We assume that there are no derivation nodes in
  * the parsed file, i.e. that we only have our axioms and conjectures.
  */
trait ProblemParserTFFTPTP
  extends BaseParserTPTP {

  def problemParser : Parser[TFFProblem] = TPTP_file ^^ generateProblem

  def generateProblem(directives : List[TPTPDirective]) : TFFProblem = {
    val onlyFormulas : List[TPTPDirective]       = directives filter isSimpleFormula
    val formulas   : List[(String,String,E)]     = onlyFormulas map extractFormulas
    val statements : List[TFFProblemStatement]   = formulas map ((t : (String,String,E)) => formulaToStatement(t._1,t._2,t._3))
    TFFProblem(statements,getSeenVars)
  }

  def isSimpleFormula(directive: TPTPDirective) : Boolean = directive match {
    case AnnotatedFormula(language,name,role,SimpleFormula(formula),_) if language == lexical.TFF.chars => true
    case _                                                                                              => false
  }

  def extractFormulas(directive : TPTPDirective) : (String,String,E) =
    directive match {
      case AnnotatedFormula(language,name,role,SimpleFormula(formula),_) if language == lexical.TFF.chars => (name,role,formula)
      case _                                                                                              => throw new Exception("Unexpected Format")
    }

  def formulaToStatement(name : String, role : String, formula : E) : TFFProblemStatement =
    role match {
      case "conjecture"         => TFFConjectureStatement(name,formula)
      case "negated_conjecture" => TFFNegatedConjectureStatement(name,formula)
      case _                    => TFFAxiomStatement(name,formula)
    }


  def problem(fileName : String) : TFFProblem = extract(fileName,problemParser)

}

class TFFProblem(val statements : List[TFFProblemStatement], val variables : Set[Sym]) {
  override def toString : String = statements.mkString("\n") + "\nVariables: " + variables.mkString(",")
}
object TFFProblem {
  def apply(statements: List[TFFProblemStatement],variables : Set[Sym]): TFFProblem = new TFFProblem(statements,variables)
}

abstract class TFFProblemStatement
case class TFFAxiomStatement(name : String, formula : E) extends TFFProblemStatement {
  override def toString : String = "tff("+name + ",axiom," + formula.toString + ")"
}
case class TFFConjectureStatement(name : String, formula : E) extends TFFProblemStatement {
  override def toString : String = "tff("+name + ",conjecture," + formula.toString + ")"
}
case class TFFNegatedConjectureStatement(name : String, formula : E) extends TFFProblemStatement {
  override def toString : String = "tff("+name + ",negated_conjecture," + formula.toString + ")"
}
