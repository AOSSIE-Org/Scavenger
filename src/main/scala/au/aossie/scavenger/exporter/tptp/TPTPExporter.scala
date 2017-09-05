package au.aossie.scavenger.exporter.tptp

import java.io.Writer

import au.aossie.scavenger.exporter.BasicFileExporter
import au.aossie.scavenger.expression.E
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.cr._
import au.aossie.scavenger.structure.immutable.Clause
import au.aossie.scavenger.util.io.Output

/**
  * Created by vlad107 on 7/5/17.
  */
class TPTPExporter(out: Output) extends BasicFileExporter(out) {
  override def extension: String = "s"

  override def write(e: E): Unit = out.write(e.toString)

  override def write(s: Clause): Unit = out.write(s.toString)

  override def write(p: Proof[CRProofNode]): Unit = {
    var sequenceNumber = 0
    p.foldDown[Int] { (node, r: Seq[Int]) =>
      sequenceNumber += 1
      // TODO: can be not only axiom, but for example conjecture
      val formula_role = node match {
        case p: InitialStatement => "axiom"
        case _ => "plain"
      }
      val ruleName = node match {
        case ConflictDrivenClauseLearning(_) =>
          "conflict-driven-clause-learning"
        case Conflict(_, _) =>
          "conflict"
        case UnitPropagationResolution(_, _, _, _, _) =>
          "unit-propagation-resolution"
        case InitialStatement(_) =>
          "axiom"
        case Decision(_) =>
          "decision"
        case Expertise(_, _) =>
          "expertise"
      }
      val status = node match {
        case p: InitialStatement => "status(thm)"
        case _ => ""
      }
      out.write(s"cnf($sequenceNumber,$formula_role,\n" +
        s"    ${node.conclusion.toTPTPString},\n" +
        s"    inference($ruleName,[$status],[${r.mkString(",")}]).\n")
      sequenceNumber
    }
  }

  override def write(a: Any): Unit = out.write(a.toString)
}
