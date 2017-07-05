package au.aossie.scavenger.exporter.tptp

import au.aossie.scavenger.exporter.BasicFileExporter
import au.aossie.scavenger.expression.E
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.cr.{Axiom, Decision, CRProofNode, Conflict, ConflictDrivenClauseLearning, UnitPropagationResolution}
import au.aossie.scavenger.structure.immutable.Clause

import scala.collection.mutable

/**
  * Created by vlad107 on 7/5/17.
  */
class TPTPFileExporter(filename: String) extends BasicFileExporter(filename) {
  override def extension: String = "s"

  override def write(e: E): Unit = write(e.toString)

  override def write(s: Clause): Unit = write(s.toString)

  override def write(p: Proof[CRProofNode]): Unit = {
    var sequenceNumber = 0
    p.foldDown[Int] { (node, r: Seq[Int]) =>
      sequenceNumber += 1
      // TODO: store this status from input
      val formula_role = node match {
        case p: Axiom => "axiom"
        case _ => "plain"
      }
      val ruleName = node match {
        case ConflictDrivenClauseLearning(_) =>
          "conflict-driven-clause-learning"
        case Conflict(_, _) =>
          "conflict"
        case UnitPropagationResolution(_, _, _, _, _) =>
          "unit-propagation-resolution"
        case Axiom(_) =>
          "axiom"
        case Decision(_) =>
          "decision"
      }
      write(s"cnf($sequenceNumber,$formula_role,\n" +
        s"    ${node.conclusion},\n" +
        s"    inference($ruleName,[],[${r.mkString(",")}]).\n")
      sequenceNumber
    }
    //      write(s"cnf($sequenceNumber,$,\n")
  }
}
