package au.aossie.scavenger

import java.io.File
import java.io.FileReader
import java.io.BufferedReader

import scalax.chart.api._
import org.jfree.chart.renderer.xy._
import org.jfree.chart.axis._
import java.awt.Color

import scala.collection.mutable._
import scala.language.postfixOps

/**
  * @author Bruno Woltzenlogel Paleo
  */
object StarExecOutputAnalysis {

  case class Config(dir: Option[String] = None,
                    solvers: Seq[String] = Seq("EP-Scavenger","TD-Scavenger","PD-Scavenger"),
                    desiredOutputStatus: String = "Unsatisfiable",
                    sotacThreshold: Int = 15,
                    verbosity: Int = 0)

  val colorer = Map(
    "LEO-II-1.7.0" -> Color.RED,
    "PD-Scavenger" -> Color.BLUE,
    "ZenonModulo-0.4.1" -> Color.GREEN,
    "Geo-III-2016C" -> Color.YELLOW,
    "SOS-2.0" -> Color.PINK,
    "Otter-3.3" -> Color.CYAN,
    "EP-Scavenger" -> Color.DARK_GRAY,
    "Beagle-SAT-0.9.47" -> Color.MAGENTA,
    "E-KRHyper-1.4" -> Color.LIGHT_GRAY,
    "TD-Scavenger" -> Color.ORANGE,
    "Zipperpin-FOF-0.4" -> new Color(0x8b0000),
    "Beagle-0.9.47" -> new Color(0xdaa520),
    "Prover9-1109a" -> new Color(0xa0db8e),
    "Metis-2.3" -> new Color(0x31698a),
    "DarwinFM-1.4.5" -> new Color(0x794044),
    "SNARK-20120808r022" -> new Color(0x191970),
    "Bliksem-1.12" -> new Color(0x6dc066),
    "PEPR-0.0ps" -> new Color(0x0e2f44),
    "GrAnDe-1.1" -> new Color(0xffc3a0),
    "CVC4-FOF-1.5.1" -> new Color(0x468499),
    "E-Darwin-1.5" -> new Color(0x008080),
    "Paradox-3.0" -> new Color(0xffe4e1),
    "ET-0.2" -> new Color(0xffd700),
    "E-2.0" -> new Color(0xd3ffce),
    "Z3-4.4.1" -> new Color(0xff7373),
    "Darwin-1.4.5" -> new Color(0xb0e0e6),
    "VampireZ3-1.0" -> new Color(0xe6e6fa),
    "Vampire-4.1" -> new Color(0x7fffd4),
    "Vampire-SAT-4.1" -> new Color(0x3399ff),
    "iProver-2.5" -> new Color(0x6897bb),
    "Vampire-4.0" -> new Color(0xff4444),
    "Vampire-SAT-4.0" -> new Color(0x8a2be2)
  ).withDefaultValue(Color.BLACK)

  val parser = new scopt.OptionParser[Config]("starexec-analysis") {
    head("\nStarExec Output Analysis\n\n")

    opt[String]('d', "directory") action { (v, c) =>
      c.copy(dir = Some(v))
    } text s"use <dir> as the directory containing StarExec's job output\n" valueName "<dir>"
    
    opt[String]('s', "solver") unbounded () action { (v, c) =>
      c.copy(solvers = c.solvers :+ v)
    } text "do detailed analysis for <solver>" valueName "<solver>"

    opt[String]('o', "desiredOutputStatus") action { (v, c) =>
      c.copy(desiredOutputStatus = v)
    } text s"consider successful those job pairs with status <desiredOutput>\n" valueName "<desiredOutput>"
    
    opt[String]('t', "sotacThreshold") action { (v, c) =>
      c.copy(sotacThreshold = v.toInt)
    } text s"find benchmarks solved by the solvers of interest and that were solved by at most <threshold> solvers\n" valueName "<threshold>"
    
    opt[String]('v', "verbosity") action { (v, c) =>
      c.copy(verbosity = v.toInt)
    } text s"set verbosity to <verbosity level>. Possible values are {0, 1}. Default = 0.\n" valueName "<verbosity level>"
    
  }

  case class JobPair(domain: String, prover: String, problem: String,
                     result: String, wallclockTime: Double, cpuTime: Double)
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
          val prover = (proverAux(0) + (if (proverAux(1) != "default" && proverAux(1) != "default2" && proverAux(0) != proverAux(1)) "_" + proverAux(1) else "")).replace("---", "-").replace("Beagle-ALL-0.9.47_","").replace("Darwin-1.4.5_","").replace("Zipperpin-0.4_","")
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
              if (c.verbosity > 0) println(jp)
              jpa += jp
              hasOutput = true 
            }
          }
          
          if (!hasOutput) {
            val tt = line.split("\t")(0).split("/")
            val jp = JobPair(domain, prover, problem, "NoOutput", tt(0).toDouble, tt(1).toDouble) 
            if (c.verbosity > 0) println(jp)
            jpa += jp
          }
          
          bf.close()
        }
        
        
        val fjpa = jpa filter { jp => jp.result != "NoOutput" && 
                                      jp.result != "ResourceOut" &&
                                      jp.result != "Unknown" &&
                                      jp.result != "GaveUp" &&
                                      jp.result != "TimeOut" &&
                                      jp.result != "Inappropriate" &&
                                      ! (jp.prover contains "Prover9Plus")
                              }
        
        
        val gfjpa = fjpa groupBy { jp => jp.prover } 
        
        // Calculate number of problems solved under a given time
        val ppt = (for ((p, pjpa) <- gfjpa) yield {
          val sortedSolvedBenchs = pjpa filter { jp => jp.result == c.desiredOutputStatus }  sortWith { (jp1, jp2) => jp1.cpuTime < jp2.cpuTime }
          
          val numOfProblemsPerTime = sortedSolvedBenchs.zipWithIndex map { case (jp, i) => (i+1, jp.cpuTime) }
          
          (p, numOfProblemsPerTime)
        }) toSeq

        
        // Plot number of problems solved under a given time. Each prover is a different line in the chart.
        val chart = XYLineChart( ppt filter { e => e._2.nonEmpty } sortWith { (e1, e2) => e1._2.length < e2._2.length } map { case (p, pt) => (p -> pt) } )
        for (i <- 0 until chart.plot.getDataset.getSeriesCount) {
          val name = chart.plot.getDataset.getSeriesKey(i).asInstanceOf[String]
          chart.plot.getRenderer.setSeriesPaint(i, colorer(name))
        }
        chart.plot.getDomainAxis.setLabel("Number of Problems")
        chart.plot.getRangeAxis.setLabel("Time (seconds)")
        chart.plot.setBackgroundPaint(Color.WHITE)
        chart.plot.setDomainGridlinePaint(Color.BLACK)
        chart.plot.setRangeGridlinePaint(Color.BLACK)
        chart.show()
        val date = new java.text.SimpleDateFormat("yyyy-MM-dd--HH-mm-ss").format(new java.util.Date())
        chart.saveAsPNG(s"${d}chart--${date}.png", (720,450))
   
        
        // Rank problems by difficulty
        val gpfjpa = (fjpa groupBy { jp => jp.problem } toSeq) sortWith 
                     { (p1, p2) => (p1._2.length < p2._2.length) || 
                                   ((p1._2.length == p2._2.length) && (p1._2 map {_.cpuTime} reduce(_ + _)) < (p2._2 map {_.cpuTime}  reduce(_ + _) ) ) } // FIXME: mapping and reducing the same thing multiple times. This is inneficient.
        
        val ranking = gpfjpa.zipWithIndex map {case ((problem, _), index) => (problem -> index)} toMap
        
        // Print problem ranking
        println("Problems Ranked by Difficulty")
        for (p <- gpfjpa.zipWithIndex) { println(p._2 + ": \t" + p._1._1 + "\t" + p._1._2.length + "\t" + (p._1._2 map {_.cpuTime} reduce {_+_})) }  // FIXME: mapping and reducing the same thing multiple times. This is inneficient.
        
        for (s <- c.solvers) {
          println()
          println(s"Problems not solved by $s but solved by at least one other prover")
          println("==========")
          gpfjpa.zipWithIndex filter { case ((problem, jpa), index) => !jpa.exists(jp => jp.prover contains s) } foreach { p => println(p._2 + ": \t" + p._1._1) }
        }
        
        val igpfjpa = gpfjpa.zipWithIndex map { p => (p._2, p._1._1, p._1._2) } // Prepend index in tuple
        val data = igpfjpa flatMap { p => p._3 map { jp => (jp,p._1,jp.cpuTime) } } groupBy { _._1.prover } map { case (p, pjps) => (p -> (pjps map { case (jp, pnumber, time) => (pnumber,time) })) } toSeq    
        
        // Scatter plot of solving time per problem
        val scatter = XYLineChart( data sortWith { (pd1, pd2) => (c.solvers contains pd1._1) || (pd1._1 compareTo pd2._1) < 0 } )
        scatter.plot.setRenderer(new XYLineAndShapeRenderer(false, true))
        scatter.plot.setRangeAxis(new LogAxis("Time (seconds)"))
        scatter.plot.getRangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
        scatter.plot.getRangeAxis.setLabel("Time (seconds)")
        scatter.plot.getDomainAxis.setLabel("Problems (ordered by decreasing difficulty)")
        scatter.plot.getRangeAxis.setLabelFont(scatter.plot.getDomainAxis.getLabelFont)
        scatter.plot.setBackgroundPaint(Color.WHITE)
        scatter.plot.setDomainGridlinePaint(Color.BLACK)
        scatter.plot.setRangeGridlinePaint(Color.BLACK)
        scatter.show()
        scatter.saveAsPNG(s"${d}scatter--${date}.png", (720,450))
        
        // Rank solvers by number of problems solved
        println()
        println("Ranking of Solvers")
        println("==================")
        ppt map { e => (e._1, e._2.length)} sortWith { (e1, e2) => e1._2 < e2._2 } foreach { println(_) }
           
        // Find problems that are solved only by one prover
        println()
        println("Problems Solved by only one Solver")
        println("==================================")
        fjpa filter { _.result == c.desiredOutputStatus } groupBy { _.problem } filter { case (problem, list) => list.length == 1 } map {case (problem, list) => println(problem + " : " + list(0).prover) }
        
        
        // Find problems that are solved by the solvers in c.solved and by only at most c.sotacThreshold solvers
        println()
        println("Problems Solved by the Solvers of Interest and by only a few other Solvers")
        println("==========================================================================")       
        (fjpa filter { _.result == c.desiredOutputStatus } groupBy { _.problem } filter { case (problem, list) => list.exists( jp => c.solvers contains jp.prover) }).toSeq sortWith { 
          (e1, e2) => e1._2.length > e2._2.length 
        } filter {
          case (problem, list) => list.length <= c.sotacThreshold
        } map {
          case (problem, list) => println(problem + ": " + (list map { jp => (jp.prover, jp.cpuTime) }).mkString(", ") )
        }
        
        // Find problem on which the solvers in c.solvers are the fastest
        println()
        println("Problems on which the Solvers of Interest are the fastest")
        println("=========================================================")          
        fjpa filter { _.result == c.desiredOutputStatus } groupBy { _.problem } map {
          case (problem, list) => { (problem, list.sortWith( (jp1, jp2) => jp1.cpuTime < jp2.cpuTime ).head )
            
            //list.nonEmpty && (c.solvers contains list.sortWith( (jp1, jp2) => jp1.cpuTime < jp2.cpuTime ).head.prover) 
          }
        } filter {
          case (problem, jp) => c.solvers contains jp.prover
        } foreach { case (problem, jp) => println(problem + ":\t" + ranking(problem) + "\t:\t" + jp.prover)}
      }

    }
  }
  // scalastyle:on
}

