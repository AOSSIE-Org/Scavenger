package au.aossie.scavenger.parser

import ammonite.ops.pwd
import au.aossie.scavenger.parser.TPTP.FOF

/**
  * Created by vlad107 on 3/3/17.
  */
object FOFTest {
  def main(args: Array[String]):Unit = {
    val path = pwd / 'examples / 'problems / 'FOF / "FOF.fofp"
    TPTPFOFParser.parse(path)
  }
}
