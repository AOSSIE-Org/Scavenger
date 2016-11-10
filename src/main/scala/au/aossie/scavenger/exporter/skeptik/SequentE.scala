package au.aossie.scavenger.exporter
package skeptik

import au.aossie.scavenger.judgment.Sequent

trait SequentE extends ExpressionE {
  def write(s: Sequent): Unit = write(s.toString)
}