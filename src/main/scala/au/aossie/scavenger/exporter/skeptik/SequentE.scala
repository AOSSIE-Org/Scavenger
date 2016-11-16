package au.aossie.scavenger.exporter
package skeptik

import au.aossie.scavenger.structure.AbstractClause

trait SequentE extends ExpressionE {
  def write(s: AbstractClause): Unit = write(s.toString)
}
