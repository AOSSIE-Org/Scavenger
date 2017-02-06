package au.aossie.scavenger

import java.io.File
import java.io.FileReader
import java.io.BufferedReader

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
                     result: String, cpuTime: String, wallclockTime: String)
  
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
          
          var result: Option[String] = None
          var cpuTime: Option[String] = None
          var wallclockTime: Option[String] = None
          var line = ""
          
          //println(lines.hasNext)
          
          while ((result == None) && lines.hasNext()) {
            line = lines.next()
            if (line.contains("SZS status")) {
              //println(line)
              val tl = line.split("\t")
              val times = tl(0).split("/")
              cpuTime = Some(times(0))
              wallclockTime = Some(times(1))
              result = "(Satisfiable|Unsatisfiable|GaveUp|TimeOut|ResourceOut)".r findFirstIn tl(1)
              val jp = JobPair(domain, prover, problem, result.get, cpuTime.get, wallclockTime.get) 
              println(jp)
              jpa += jp
            }
          }
          
          if (result == None) {
            //println(line)
            val tl = line.split("\t")
            val times = tl(0).split("/")
            cpuTime = Some(times(0))
            wallclockTime = Some(times(1))
            val jp = JobPair(domain, prover, problem, "ResourceOut", cpuTime.get, wallclockTime.get) 
            println(jp)
            jpa += jp            
          }

        }
      }
    }
  }
}
