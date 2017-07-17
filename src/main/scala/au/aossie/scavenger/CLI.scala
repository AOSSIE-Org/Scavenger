package au.aossie.scavenger

import ammonite.ops._
import au.aossie.scavenger.structure.immutable.CNF
import au.aossie.scavenger.prover.{EPCR, PDCR, ProblemStatus, Satisfiable, TDCR, Unsatisfiable}
import au.aossie.scavenger.parser.{TPTPCNFParser, TPTPFOFParser}
import au.aossie.scavenger.expression.{Abs, App, E, Sym}
import au.aossie.scavenger.util.io.{Output, StandardOutput}
import au.aossie.scavenger.exporter.tptp.TPTPExporter
import au.aossie.scavenger.proof.Proof

import scala.util.{Try,Success,Failure}
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * @author Daniyar Itegulov
  */
object CLI {

  case class Config(inputs: Seq[String] = Seq(),
                    configuration: String = "EP",
                    format: Option[String] = None,
                    output: Output = StandardOutput,
                    dependenciesDir: Option[Path] = None)

  val configurations = Map(
    "PD" -> Seq(PDCR),
    "EP" -> Seq(new EPCR(100, 10, 10000, 1.0, 0.99, 1e10, 10, false),
                new EPCR(100, 10, 10000, 1.0, 0.99, 1e10, 5, true)),
    "TD" -> Seq(TDCR)
  )
  val parsers = Map(
    "cnf"  -> TPTPCNFParser,
    "cnfp" -> TPTPCNFParser,
    "fof"  -> TPTPFOFParser
  )
  val knownFormats = Seq("cnf", "cnfp", "fof")

  val parser = new scopt.OptionParser[Config]("scavenger") {
    head("\nScavenger's Command Line Interface\n\n")

    opt[String]('a', "algorithm") action { (v, c) =>
      c.copy(configuration = v)
    } text "use <alg> to solve the problem" valueName "<alg>"

    opt[String]('d', "dependencies") action { (v, c) =>
      c.copy(dependenciesDir = Some(pwd / RelPath(v)))
    }

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

  // FIXME: This is not the right place for this function
//  def getUppercaseVariables(cnf: CNF): mutable.Set[Sym] = {
//    def uppercaseVariableInFormula(e: E): Set[Sym] = e match {
//      case v: Sym if v.name.head.isUpper => Set(v)
//      case App(l, r)                     => uppercaseVariableInFormula(l) ++ uppercaseVariableInFormula(r)
//      case Abs(_, _, body)               => uppercaseVariableInFormula(body)
//      case _                             => Set.empty
//    }
//    val variables = mutable.Set.empty[Sym]
//    cnf.clauses
//      .flatMap(clause => clause.ant ++ clause.suc)
//      .foreach(variables ++= uppercaseVariableInFormula(_))
//    variables
//  }

  
  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) foreach { c =>
      val solvers = configurations(c.configuration)
      for (input <- c.inputs) {
        val parser = parsers.getOrElse(c.format.getOrElse(input.split('.').last), TPTPCNFParser)
        val path   = Path.apply(input, pwd)
        val cnf    = parser.parse(path, c.dependenciesDir)
        val problemName = input.drop(input.lastIndexOf("/") + 1)
        implicit val ec: ExecutionContext = ExecutionContext.global
        val futures = solvers.map { solver =>
          Future[ProblemStatus] {
            solver.prove(cnf)
          }
        }
        val firstCompleted = Future.firstCompletedOf(futures)
        Await.ready(firstCompleted, Duration.Inf)
        firstCompleted.value match {
          case Some(Success(problemStatus)) => problemStatus match {
            case Unsatisfiable(Some(p)) =>
              
              for (c <- cnf.clauses) println(c.tp)
              
              //FIXME: This is a hack to obtain "Theorem" instead of "Satisfiable"
              def hasConjecture(cnf: CNF): Boolean = {
                scala.io.Source.fromFile(input).getLines().exists { _ contains ",conjecture," }
              } 
              val status = if (hasConjecture(cnf)) "Theorem" else "Unsatisfiable"
              
              c.output.write(s"% SZS status $status for $problemName\n")
              c.output.write(s"% SZS output start CNFRefutation for $problemName\n")
              new TPTPExporter(c.output).write(p)
              c.output.write(s"% SZS output end CNFRefutation for $problemName\n")
            case Satisfiable(m) =>
              c.output.write(s"% SZS status Satisfiable for $input")
              c.output.write(s"% SZS output start Model for $problemName\n")
              c.output.write(m)
              c.output.write(s"% SZS output end Model for $problemName\n")
            case _ =>
              c.output.write(s"% SZS status GaveUp for $input")
          }
          case _ => 
        }
      }
    }
  }
}
