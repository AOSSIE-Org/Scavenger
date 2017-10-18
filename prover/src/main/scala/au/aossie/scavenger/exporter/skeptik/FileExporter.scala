package au.aossie.scavenger.exporter
package skeptik

import au.aossie.scavenger.util.io.FileOutput

class FileExporter(filename: String, val deletionInfo: Boolean = false)
  extends BasicFileExporter(new FileOutput(filename)) with ExpressionE with SequentE with ProofE {
  def extension = if (deletionInfo) "sd" else "s"
  def write(a: Any): Unit = throw new IllegalArgumentException
}

