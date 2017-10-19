package org.aossie.scavenger.exporter
package skeptik

import org.aossie.scavenger.expression.E

trait ExpressionE extends Exporter {
  def write(e: E): Unit = write(e.toString)
}
