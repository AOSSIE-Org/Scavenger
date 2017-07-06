package au.aossie.scavenger.exporter

import java.io.Writer

import au.aossie.scavenger.util.io.Output

abstract class BasicFileExporter(w: Output) extends Exporter {
  def extension: String
  def write(s: Array[Char], off: Int, len: Int) = w.write(s,off,len)
}
