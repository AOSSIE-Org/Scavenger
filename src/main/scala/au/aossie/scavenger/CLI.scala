package au.aossie.scavenger

import ammonite.ops._
import au.aossie.scavenger.structure.immutable.CNF
import au.aossie.scavenger.prover.{CR, EPCR, Satisfiable, TDCR, Unsatisfiable}
import au.aossie.scavenger.parser.TPTPCNFParser
import au.aossie.scavenger.expression.{Abs, App, E, Sym}
import au.aossie.scavenger.util.io.{Output, StandardOutput}

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
object CLI {

  case class Config(inputs: Seq[String] = Seq(),
                    algorithm: String = "CR",
                    format: Option[String] = None,
                    output: Output = StandardOutput)

  val configurations = Map(
    "PD" -> CR,
    "EP" -> EPCR,
    "TD" -> TDCR
  )
  val parsers = Map(
    "cnf"  -> TPTPCNFParser,
    "cnfp" -> TPTPCNFParser
  )
  val knownFormats = Seq("cnf", "cnfp")

  val parser = new scopt.OptionParser[Config]("scavenger") {
    head("\nScavenger's Command Line Interface\n\n")

    opt[String]('a', "algorithm") action { (v, c) =>
      c.copy(configuration = v)
    } text "use <alg> to solve the problem" valueName "<alg>"

    note(
      s"""
        <alg> can be any of the following algorithms:
        ${configurations.keys.mkString(", ")}
        """
    )

    opt[String]('f', "format") action { (v, c) =>
      c.copy(format = Some(v))
    } validate { v =>
      if (knownFormats contains v) success
      else failure("unknown problem format: " + v)
    } text s"use <format> for input problem\n" valueName "<format>"

    note(
      s"""
        <format> can be any of the following:
        ${knownFormats.mkString(", ")}
        """
    )

    opt[String]('o', "out") action { (v, c) =>
      c.copy(output = Output(v))
    } text "output proof to <file>\n" valueName "<file>"

    arg[String]("<problem-file>...") unbounded () optional () action { (v, c) =>
      c.copy(inputs = c.inputs :+ v)
    } text "solve <problem-file>\n"

    help("help") text "print this usage text"

    note("""
    Example:
      The following command solves the problem 'SET006-1.cnfp'
      using the algorithm 'ConcurrentCR'.
      The clause set is unsatisfiable and
      the refutation is written to 'refutation.proof'.

      scavenger -a ConcurrentCR -f cnfp -o refutation.proof examples/problems/CNF/SET006-1.cnfp
      """)
  }

  // TODO: This is not the right place for this function
  def getUppercaseVariables(cnf: CNF): mutable.Set[Sym] = {
    def uppercaseVariableInFormula(e: E): Set[Sym] = e match {
      case v: Sym if v.name.head.isUpper => Set(v)
      case App(l, r)                     => uppercaseVariableInFormula(l) ++ uppercaseVariableInFormula(r)
      case Abs(_, _, body)               => uppercaseVariableInFormula(body)
      case _                             => Set.empty
    }
    val variables = mutable.Set.empty[Sym]
    cnf.clauses
      .flatMap(clause => clause.ant ++ clause.suc)
      .foreach(variables ++= uppercaseVariableInFormula(_))
    variables
  }

  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) foreach { c =>
      val solver = configurations(c.configuration)
      for (input <- c.inputs) {
        val parser = parsers.getOrElse(c.format.getOrElse(input.split('.').last), TPTPCNFParser)
        val path   = Path.apply(input, pwd)
        val cnf    = parser.parse(path)
        solver.prove(cnf) match {
          case Unsatisfiable(p) =>
            c.output.write(s"% SZS status Unsatisfiable for $input")
            c.output.write("\n")
            c.output.write(p)
          case Satisfiable(m) =>
            c.output.write(s"% SZS status Satisfiable for $input")
            c.output.write("\n")
            c.output.write(m)
          case _ =>
            c.output.write(s"% SZS status GaveUp for $input")
        }
      }
    }
  }
}
