package au.aossie.scavenger.util

import java.io._

import scala.collection.mutable.ListBuffer

package object io {
  abstract class Output {
    def write(s: Any): Unit
  }
  object Output {
    def apply(path: String): Output =
      if (path contains "stdout://") StandardOutput
      else if (path contains "void://") NoOutput
      else new FileOutput(path)
  }

  object NoOutput extends Output {
    def write(s: Any): Unit = {}
  }

  object StandardOutput extends Output {
    def write(s: Any): Unit = print(s.toString)
  }

  class FileOutput(path: String) extends Output {
//    private val w = Resource.fromFile(path)
    private val w = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path)))

//    def clear(): Unit         = w.truncate(0)
//    def prepend(s: Any): Unit = w.insert(0, s.toString)
//    def appendAll(strings: Traversable[String], separator: String = ""): Unit =
//      w.appendStrings(strings, separator)
//    def isEmpty: Boolean    = w.lines().isEmpty
    def write(s: Any): Unit = w.append(s.toString)
  }

  class Input(path: String) {
//    private val r = Resource.fromFile(path)
    private val r = new BufferedReader(new InputStreamReader(new FileInputStream(path)))

    def lines: Traversable[String] = {
      val it = r.lines().iterator()
      val buffer = ListBuffer.empty[String]
      while (it.hasNext) {
        buffer.append(it.next())
      }
      buffer
    }
  }
  object Input {
    def apply(path: String) = new Input(path)
  }

}
