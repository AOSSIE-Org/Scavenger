package org.aossie.scavenger.exporter

import org.aossie.scavenger.util.io.Output

abstract class BasicFileExporter(w: Output) extends Exporter {
  def extension: String
  def write(s: Array[Char], off: Int, len: Int) = w.write(s.slice(off, off + len))
}
