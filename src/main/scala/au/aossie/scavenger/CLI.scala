package au.aossie.scavenger

import ammonite.ops._
import au.aossie.scavenger.prover.{EPCR, ExpertProver, PDCR, ProblemStatus, Satisfiable, TDCR, Unsatisfiable}
import au.aossie.scavenger.parser.{TPTPCNFParser, TPTPFOFParser}
import au.aossie.scavenger.util.io.{Output, StandardOutput}
import au.aossie.scavenger.exporter.tptp.TPTPExporter

import scala.util.Success
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * @author Daniyar Itegulov
  */
object CLI {

  case class Config(inputs: Seq[String] = Seq(),
                    format: Option[String] = None,
                    configuration: String = "EP",
                    includesDir: Option[Path] = None,
                    output: Output = StandardOutput)

  val configurations = Map(
    "PD" -> Seq(PDCR),
    "EP" -> Seq(new EPCR(100, 10, 1.0, 0.99, 1e10, 10, false),
                new EPCR(100, 10, 1.0, 0.99, 1e10, 5, true)),
    "TD" -> Seq(TDCR),
    "Expert" -> Seq(new ExpertProver(4, true, 6))
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
    
    opt[String]('d', "directory") action { (v, c) =>
      c.copy(includesDir = Some(pwd / RelPath(v)))
    } text "directory where included files are located"


    opt[String]('f', "format") action { (v, c) =>
      c.copy(format = Some(v))
    } text "format of the input file(s). Either 'cnf' or 'fof'"
         
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
        
        val filePath = Path(input, pwd)
        val includesDir = c.includesDir.getOrElse(filePath / up)
        
        val parser = {
          c.format match {
            case Some("cnf") => new TPTPCNFParser(includesDir)
            case Some("fof") => new TPTPFOFParser(includesDir)
            case _ => scala.io.Source.fromFile(input).getLines() find { l => (l contains "cnf(") || (l contains "fof(") } match {
              case Some(l) if (l contains "cnf(") => new TPTPCNFParser(includesDir)
              case Some(l) if (l contains "fof(") => new TPTPFOFParser(includesDir)
              case _ => throw new Exception("unknown file format") 
            }
          }
        } 

        
        val cnf    = parser.parse(filePath)
        
        implicit val ec: ExecutionContext = ExecutionContext.global
        val futures = solvers.map { solver =>
          Future[ProblemStatus] {
            solver.prove(cnf)
          }
        }
        val firstCompleted = Future.firstCompletedOf(futures)
        Await.ready(firstCompleted, Duration.Inf)
        
        //FIXME: This is a hack to obtain "Theorem" instead of "Unsatisfiable" and "CounterSatisfiable" instead of "Satisfiable"
        def hasConjecture(input: String): Boolean = {
          val result = scala.io.Source.fromFile(input).getLines().exists { _ contains ",conjecture," }
          println(s"hasConjecture: $result")
          result
        } 
        
        val problemName = input.drop(input.lastIndexOf("/") + 1)
        
        firstCompleted.value match {
          case Some(Success(problemStatus)) => problemStatus match {
            case Unsatisfiable(Some(p)) =>
              val status = if (hasConjecture(input)) "Theorem" else "Unsatisfiable"   
              c.output.write(s"% SZS status $status for $problemName\n")
              c.output.write(s"% SZS output start CNFRefutation for $problemName\n")
              new TPTPExporter(c.output).write(p)
              c.output.write(s"% SZS output end CNFRefutation for $problemName\n")
              sys.exit(0)
            case Satisfiable(m) =>
              val status = if (hasConjecture(input)) "CounterSatisfiable" else "Satisfiable"  
              c.output.write(s"% SZS status $status for $problemName\n")
              c.output.write(s"% SZS output start Model for $problemName\n")
              c.output.write(m)
              c.output.write("\n")
              c.output.write(s"% SZS output end Model for $problemName\n")
              sys.exit(0)
            case _ =>
              c.output.write(s"% SZS status GaveUp for $input")
              sys.exit(0)
          }
          case _ => 
        }
      }
    }
  }
}
