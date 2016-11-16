package au.aossie.scavenger.parser.TPTP

import au.aossie.scavenger.expression.{E, Sym}
import au.aossie.scavenger.parser.TPTP.TPTPAST.{AnnotatedFormula, SimpleSequent, TPTPDirective}

import collection.mutable.Set
/**
  * @author  Ezequiel Postan
  * @since   24.05.2016
  * @version 1.0
  * @note    This version accepts only CNF formulas that are taken as axioms
  *          except for a conjecture or negated conjecture. No derivation nodes
  *          or other TPTP annotated formulas are accepted.
  */
object CNF extends CNF

/**
  * The ProblemParserFOFTPTP trait implements a parser for problems written
  * in the TPTP CNF syntax. We assume that there are no derivation nodes in
  * the parsed file, i.e. that we only have our axioms and a final conjecture.
  *
  * TODO: Add (if needed) the treatment for FOF formulas and skolemization steps
  */
trait CNF
extends Base {

  def problemParser : Parser[CNFProblem] = TPTP_file ^^ generateProblem

  def generateProblem(directives : List[TPTPDirective]) : CNFProblem = {
    val formulas   : List[(String,String,Seq[E],Seq[E])] = directives map extractFormulas
    val statements : List[CNFProblemStatement]           = formulas map ((t : (String,String,Seq[E],Seq[E])) => formulaToStatement(t._1,t._2,t._3,t._4))
    CNFProblem(statements,getSeenVars)
  }


  def extractFormulas(directive : TPTPDirective) : (String,String,Seq[E],Seq[E]) =
    directive match {
      case AnnotatedFormula(language,name,role,SimpleSequent(ant,suc),_) if language == lexical.CNF.chars => (name,role,ant,suc)
      case _                                                                                              => throw new Exception("Unexpected Format")
    }

  def formulaToStatement(name : String, role : String, ant : Seq[E], suc : Seq[E]) : CNFProblemStatement =
    role match {
      case "conjecture"         => CNFConjectureStatement(name,ant.toList,suc.toList)
      case "negated_conjecture" => CNFNegatedConjectureStatement(name,ant.toList,suc.toList)
      case _                    => CNFAxiomStatement(name,ant.toList,suc.toList)
    }


  def problem(fileName : String) : CNFProblem = extract(fileName,problemParser)

}

class CNFProblem(val statements : List[CNFProblemStatement],val variables : Set[Sym]) {
  override def toString : String = statements.mkString("\n") + "\nVariables: " + variables.mkString(",")
}
object CNFProblem {
  def apply(statements: List[CNFProblemStatement], variables : Set[Sym]): CNFProblem = new CNFProblem(statements,variables)
}

abstract class CNFProblemStatement
case class CNFAxiomStatement(name : String, ant : List[E], suc : List[E]) extends CNFProblemStatement {
  override def toString : String = {
    val initialNegation = if (ant.isEmpty) "" else "~ "
    "cnf(" + name + ",axiom,{ " + initialNegation + ant.mkString(", ~ ") + " --> " + suc.mkString(",") + " })"
  }
}
case class CNFConjectureStatement(name : String, ant : List[E], suc : List[E]) extends CNFProblemStatement {
  override def toString : String = {
    val initialNegation = if (ant.isEmpty) "" else "~ "
    "cnf(" + name + ",conjecture,{ " + initialNegation + ant.mkString(", ~ ") + " --> " + suc.mkString(",") + "})"
  }
}
case class CNFNegatedConjectureStatement(name : String, ant : List[E], suc : List[E]) extends CNFProblemStatement {
  override def toString : String = {
    val initialNegation = if (ant.isEmpty) "" else "~ "
    "cnf(" + name + ",negated_conjecture,{ " + initialNegation + ant.mkString(", ~ ") + " --> " + suc.mkString(",") + "})"
  }
}
