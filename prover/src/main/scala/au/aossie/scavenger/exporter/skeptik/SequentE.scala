package au.aossie.scavenger.exporter.skeptik

import au.aossie.scavenger.structure.immutable.Clause

trait SequentE extends ExpressionE {
  def write(s: Clause): Unit = write(s.toString)
}
