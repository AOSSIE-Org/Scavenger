package org.aossie.scavenger.util

package object unicode {
  private val encoding = System.getProperty("file.encoding")

  def unicodeOrElse(unicode: String, alternative: String): String =
    if (encoding == "UTF-8") unicode else alternative
}

