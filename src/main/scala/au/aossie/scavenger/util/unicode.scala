package au.aossie.scavenger.util

package object unicode {
  def unicodeOrElse(unicode: String, alternative: String) =
    if (System.getProperty("file.encoding") == "UTF-8") unicode else alternative
}
