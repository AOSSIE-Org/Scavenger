package au.aossie.scavenger.exporter
package skeptik

import au.aossie.scavenger.structure.immutable.Clause

trait SequentE extends ExpressionE {
  def write(s: Clause): Unit = write(s.toString)
}
