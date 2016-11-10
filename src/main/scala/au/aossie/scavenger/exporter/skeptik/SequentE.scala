package au.aossie.scavenger.exporter
package skeptik

import au.aossie.scavenger.structure.Sequent

trait SequentE extends ExpressionE {
  def write(s: Sequent): Unit = write(s.toString)
}