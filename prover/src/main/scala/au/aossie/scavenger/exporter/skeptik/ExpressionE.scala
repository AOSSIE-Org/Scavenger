package au.aossie.scavenger.exporter
package skeptik

import au.aossie.scavenger.expression.E

trait ExpressionE extends Exporter {
  def write(e: E): Unit = write(e.toString)
}
