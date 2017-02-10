package au.aossie.scavenger

import java.awt.Color
import java.io.File
import java.io.FileReader
import java.io.BufferedReader

import scalax.chart.api._
import scala.collection.mutable._
import scala.language.postfixOps

/**
  * @author Bruno Woltzenlogel Paleo
  */
object StarExecOutputAnalysis {

  case class Config(dir: Option[String] = None)


  val parser = new scopt.OptionParser[Config]("starexec-analysis") {
    head("\nStarExec Output Analysis\n\n")

    opt[String]('d', "directory") action { (v, c) =>
      c.copy(dir = Some(v))
    } text s"use <dir> as the directory containing StarExec's job output\n" valueName "<dir>"

  }

  case class JobPair(domain: String, prover: String, problem: String,
                     result: String, cpuTime: Double, wallclockTime: Double)
  // scalastyle:off
  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) foreach { c =>
      def walkTree(file: File): Iterable[File] = {
        val children = new Iterable[File] {
          def iterator = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
        }
        Seq(file) ++: children.flatMap(walkTree(_))
      }
      
      val jpa = new ArrayBuffer[JobPair]()
      
      for (d <- c.dir) {
        val offset = d.split("/").length
        val f0 = new File(d)
        for (f <- walkTree(f0) if !f.isDirectory && f.toString.contains(".txt") ) {
          val path = f.toString.split("/")
          val domain = path(offset)
          val proverAux = path(offset + 1).split("___")
          val prover = (proverAux(0) + (if (proverAux(1) != "default" && proverAux(0) != proverAux(1)) "_" + proverAux(1) else "")).replace("---", "-")
          val problem = path(offset + 2)
          
          val bf = new BufferedReader(new FileReader(f))
          val lines = bf.lines.iterator()

          var line = ""
          var hasOutput = false
          while (!hasOutput && lines.hasNext()) {
            line = lines.next()
            if (line.contains("SZS status")) {
              val tl = line.split("\t")
              val timePrefix = tl(0)
              val ss = tl(1).split("SZS status ")
              val status = "([a-zA-Z]+)".r findFirstIn ss(1)
              val tt = timePrefix.split("/")
              val jp = JobPair(domain, prover, problem, status.get, tt(0).toDouble, tt(1).toDouble) 
              println(jp)
              jpa += jp
              hasOutput = true 
            }
          }
          
          if (!hasOutput) {
            val tt = line.split("\t")(0).split("/")
            val jp = JobPair(domain, prover, problem, "NoOutput", tt(0).toDouble, tt(1).toDouble) 
            println(jp)
            jpa += jp
          }
          
          bf.close()
        }
        
        
        val fjpa = jpa filter { jp => jp.result != "NoOutput" }
        
        val gfjpa = fjpa groupBy { jp => jp.prover } 
        
        
        // FIXME: In general, we want to filter results that are correct, and not just "Unsatisfiable"
        // But StarExec's output doesn't tell us the expected status for each job pair.
        // We should at least get the expected status from the command line, instead of hard-coding it here.
        val SOLVED = "Unsatisfiable"
        
        // Calculating number of problems solved under a given time
        val ppt = (for ((p, pjpa) <- gfjpa) yield {
          val sortedSolvedBenchs = pjpa filter { jp => jp.result == SOLVED }  sortWith { (jp1, jp2) => jp1.cpuTime < jp2.cpuTime }
          
          //for (e <- sortedSolvedBenchs) println(p + " " + e)
          
          val numOfProblemsPerTime = sortedSolvedBenchs.zipWithIndex map { case (jp, i) => (i+1, jp.cpuTime) }
          
          //for (e <- numOfProblemsPerTime) println(p + " " + e)
          
          (p, numOfProblemsPerTime)
        }) toSeq

        val chart = XYLineChart( ppt filter { e => e._2.nonEmpty } sortWith { (e1, e2) => e1._2.length < e2._2.length } map { case (p, pt) => (p -> pt) } )
        chart.plot.getDomainAxis.setLabel("Number of Problems")
        chart.plot.getRangeAxis.setLabel("Time (seconds)")
        chart.plot.setBackgroundPaint(Color.WHITE)
        chart.plot.setDomainGridlinePaint(Color.BLACK)
        chart.plot.setRangeGridlinePaint(Color.BLACK)
        chart.show()
        val date = new java.text.SimpleDateFormat("yyyy-mm-dd--HH-mm-ss").format(new java.util.Date())
        chart.saveAsPNG(s"${d}chart--${date}.png")
        
        // Sort provers by number of problems solved
        ppt map { e => (e._1, e._2.length)} sortWith { (e1, e2) => e1._2 < e2._2 } foreach { println(_) }
        
        
        // Find problems that are solved only by one prover
        println()
        fjpa filter { _.result == SOLVED } groupBy { _.problem } filter { case (problem, list) => list.length == 1 } map {case (problem, list) => println(problem + " : " + list(0).prover) }
        
        
        // Find problems on which Scavenger is strong and most other provers aren't
        println()
        (fjpa filter { _.result == SOLVED } groupBy { _.problem } filter { case (problem, list) => list.exists( jp => jp.prover == "Scavenger 0.1-EPCR-10" ) }).toSeq sortWith { 
          (e1, e2) => e1._2.length > e2._2.length 
        } filter {
          case (problem, list) => list.length <= 8 // at most 8 other provers should have solved the same problem
        } map {
          case (problem, list) => println(); println(problem + ": " + (list map { jp => (jp.prover, jp.cpuTime) }).mkString(", ") )
        }
        
      }

    }
  }
  // scalastyle:on
}
