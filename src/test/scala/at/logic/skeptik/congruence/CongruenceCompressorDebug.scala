package at.logic.skeptik.congruence

import at.logic.skeptik.expression.formula._
import at.logic.skeptik.expression._
import at.logic.skeptik.algorithm.compressor.congruence._
import at.logic.skeptik.algorithm.compressor._
import at.logic.skeptik.algorithm.compressor.subsumption._
import at.logic.skeptik.parser.ProofParserVeriT
import at.logic.skeptik.parser.ProofParserSkeptik
import at.logic.skeptik.proof._
import at.logic.skeptik.util.io.Input
import scala.collection.mutable.{HashMap => MMap}

object CongruenceCompressorDebug {

  def main(args: Array[String]):Unit = {

    val multiple = false
    val reader = new Input("F:/Proofs/QF_UF/seq_files")
//    val file = "F:/Proofs/QF_UF/SEQ/SEQ005_size6.smt2"
//    val file = "F:/Proofs/QF_UF/SEQ/SEQ005_size8.smt2"
//      val file = "F:/Proofs/QF_UF/SEQ/SEQ017_size5.smt2"
//    val file = "F:/Proofs/QF_UF/SEQ/SEQ005_size8.smt2"
//    val file = "F:/Proofs/QF_UF/SEQ/SEQ013_size4.smt2"
//      val file = "F:/Proofs/QF_UF/SEQ/SEQ032_size2.smt2"
//    val file = "F:/Proofs/QF_UF/SEQ/SEQ010_size8.smt2"
//    val file = "F:/Proofs/QF_UF/SEQ/SEQ010_size6.smt2"
      val file = "F:/Proofs/QF_UF/SEQ/SEQ010_size8.smt2"
//    val file = "F:/Proofs/QF_UF/SEQ/SEQ015_size2.smt2"
//    val file = "experiments/congruence/resolveBug.s"
//      val file = "experiments/congruence/resolveBug10_1.smtb"
    val parser = ProofParserVeriT
//    val parser = ProofParserSkeptik
    if (multiple) {
      val lines = reader.lines
  //    var percentage: Double = - 1
      for (singleFile <- lines) {
        println("parsing " + singleFile)
        val proof = parser.read(singleFile)
        println("finished parsing")
        val newProof = FibonacciCongruence(proof)
//        val newProof2 = FibonacciCNewNew(proof)
        println(measure(proof))
        println(measure(newProof))
        println(newProof.root)
//        println(measure(newProof2))
//        println(newProof2.root)
      }
    }
    else {
      val t = System.currentTimeMillis()
      val proof = parser.read(file)
      println("finished parsing")
//      println(proof)
      val t2 = System.currentTimeMillis()
      val parseTime = t2 - t
      println("Parsing time: " + parseTime)
      val newProof = FibonacciCongruence(proof)
      val timeReq = System.currentTimeMillis() - t2
      println("time: " + timeReq + "ms")
//      println(proof)
      val measures = measure(proof)
      val newMeasures = measure(newProof)
      println(measures)
      println(newMeasures)
//      println("trans is shift: " + (measures("length") - newMeasures("length") == measures("transLength") - newMeasures("transLength")))
      println(newProof.root)
//      println(newProof)
//      println("orig after RPI: " + measure(RecyclePivotsWithIntersection(proof)))
//      println("compressed after RPI: " + measure(RecyclePivotsWithIntersection(newProof)))
    }
  }
}