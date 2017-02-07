package au.aossie.scavenger

import java.io.File
import java.io.FileReader
import java.io.BufferedReader

import scalax.chart.api._

import scala.collection.mutable._

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
          val prover = path(offset + 1).split("___")(0)
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
        
        
        // Calculating number of problems solved under a given time
        val ppt = for ((p, pjpa) <- gfjpa) yield {
          // FIXME: In general, we want to filter results that are correct, and not just "Unsatisfiable"
          val sortedSolvedBenchs = pjpa filter { jp => jp.result == "Unsatisfiable" }  sortWith { (jp1, jp2) => jp1.cpuTime < jp2.cpuTime }
          
          for (e <- sortedSolvedBenchs) println(p + " " + e)
          
          val numOfProblemsPerTime = sortedSolvedBenchs.zipWithIndex map { case (jp, i) => (i+1, jp.cpuTime) }
          
          for (e <- numOfProblemsPerTime) println(p + " " + e)
          
          (p, numOfProblemsPerTime)
        }
        
        // TODO: Make this chart more beautiful. Include axis labels in the chart. 
        // Grey is probably not a good color for the background.
        val chart = XYLineChart( ppt.toSeq map { case (p, pt) => (p -> pt.toIndexedSeq) } )
        chart.show()
        val date = new java.text.SimpleDateFormat("yyyy-mm-dd--HH-mm-ss").format(new java.util.Date())
        chart.saveAsPNG(s"${d}chart--${date}.png")        
        
        
      }

    }
  }
}
