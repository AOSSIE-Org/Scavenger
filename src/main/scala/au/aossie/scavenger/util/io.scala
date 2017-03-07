package au.aossie.scavenger.util

import scalax.io.Resource

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
    private val w = Resource.fromFile(path)

    def clear(): Unit         = w.truncate(0)
    def prepend(s: Any): Unit = w.insert(0, s.toString)
    def appendAll(strings: Traversable[String], separator: String = ""): Unit =
      w.appendStrings(strings, separator)
    def isEmpty: Boolean    = w.lines().isEmpty
    def write(s: Any): Unit = w.append(s.toString)
  }

  class Input(path: String) {
    private val r = Resource.fromFile(path)

    def lines: Traversable[String] = r.lines()
  }
  object Input {
    def apply(path: String) = new Input(path)
  }

}
