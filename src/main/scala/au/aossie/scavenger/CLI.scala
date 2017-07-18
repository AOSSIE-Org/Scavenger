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
                    output: Output = StandardOutput)

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

  val parser = new scopt.OptionParser[Config]("scavenger") {
    head("\nScavenger's Command Line Interface\n\n")

    opt[String]('a', "algorithm") action { (v, c) =>
      c.copy(configuration = v)
    } text "use <configuration> to solve the problem" valueName "<alg>"

    note(
      s"""
        <configuration> can be any of the following algorithms:
        ${configurations.keys.mkString(", ")}
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

  
  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) foreach { c =>
      val solvers = configurations(c.configuration)
      for (input <- c.inputs) {
        
        //FIXME: This is a hack to automatically detect which parser to use
        def detectParser(input: String) = {
          scala.io.Source.fromFile(input).getLines() find { l => (l contains "cnf(") || (l contains "fof(") } match {
            case Some(l) if (l contains "cnf(") => TPTPCNFParser
            case Some(l) if (l contains "fof(") => TPTPFOFParser
            case _ => throw new Exception("unknown file format") 
          }
        } 
        
        val parser = detectParser(input)
        val path   = Path.apply(input, pwd)
        val cnf    = parser.parse(path)
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
              
              //FIXME: This is a hack to obtain "Theorem" instead of "Unsatisfiable"
              def hasConjecture(cnf: CNF): Boolean = {
                scala.io.Source.fromFile(input).getLines().exists { _ contains ",conjecture," }
              } 
              val status = if (hasConjecture(cnf)) "Theorem" else "Unsatisfiable"
              
              c.output.write(s"% SZS status $status for $problemName\n")
              c.output.write(s"% SZS output start CNFRefutation for $problemName\n")
              new TPTPExporter(c.output).write(p)
              c.output.write(s"% SZS output end CNFRefutation for $problemName\n")
            case Satisfiable(m) =>
              c.output.write(s"% SZS status Satisfiable for $input\n")
              c.output.write(s"% SZS output start Model for $problemName\n")
              c.output.write(m)
              c.output.write("\n")
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
