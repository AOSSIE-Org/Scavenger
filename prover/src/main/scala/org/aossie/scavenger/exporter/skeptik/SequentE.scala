package org.aossie.scavenger.exporter.skeptik

import org.aossie.scavenger.structure.immutable.Clause

trait SequentE extends ExpressionE {
  def write(s: Clause): Unit = write(s.toString)
}
