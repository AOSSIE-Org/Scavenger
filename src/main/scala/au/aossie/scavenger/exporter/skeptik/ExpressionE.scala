package au.aossie.scavenger.exporter
package skeptik

import au.aossie.scavenger.expression.{E,Sym,Abs,App,AppRec}
import au.aossie.scavenger.expression.formula._

trait ExpressionE extends Exporter {
  def write(e: E): Unit = write(e.toString)
}