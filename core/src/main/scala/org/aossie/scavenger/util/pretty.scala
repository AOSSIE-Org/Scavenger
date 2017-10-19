package org.aossie.scavenger.util

import org.aossie.scavenger.expression._
import org.aossie.scavenger.structure.immutable.Clause
import org.aossie.scavenger.util.math._

package object pretty {
  def blankString(length: Int): String = " " * length

  def mkStringMultiLine(c: Iterable[Any], leftMargin: Int, width: Int, sep: String): String = {
    val margin    = blankString(leftMargin)
    var counter   = margin.length
    var paragraph = margin
    for (w <- c) {
      paragraph += w + sep
      counter += w.toString.length + sep.length
      if (counter > width) {
        paragraph += "\n" + margin
        counter = margin.length
      }
    }
    paragraph
  }

  def prettyTable[A](t: Seq[Seq[A]], sep: String = "   ", header: Boolean = true): String = {
    val tTrans       = t.transpose
    val widths       = t.map(_.map(_.toString.length))
    val sepWidth     = sep.length
    val columnWidths = widths.transpose.map(column => max(column, (x: Int) => x))
    val fixedWidthTableTrans = tTrans.zip(columnWidths).map { columnWidthPair =>
      val (column, width) = columnWidthPair
      column.map(e => e.toString + blankString(width - e.toString.length))
    }
    val fixedWidthTable = fixedWidthTableTrans.transpose
    val headerStr = if (header) {
      val totalWidth    = (0 /: columnWidths)((w, e) => w + e + sepWidth)
      val horizontalBar = "=" * totalWidth + "\n"
      fixedWidthTable.head.mkString("", sep, "\n") + horizontalBar
    } else {
      ""
    }
    val rows = fixedWidthTable.tail.map(row => row.mkString("", sep, "\n"))
    (headerStr /: rows)(_ + _)
  }

  def tptpPrettify(e: E): String = {
    e match {
      case Abs(_, _, _) => throw new IllegalArgumentException("Doesn't work with abs")
      case Sym("additive_identity") => "0"
      case Sym("multiplicative_identity") => "1"
      case App(Sym("multiplicative_inverse"), a) => "1/" + tptpPrettify(a)
      case App(Sym("additive_inverse"), a) => "-" + tptpPrettify(a)
      case AppRec(Sym("less_or_equal"), Seq(l, r)) => tptpPrettify(l) + " ≤ " + tptpPrettify(r)
      case AppRec(Sym("sum"), Seq(a, b, c)) => tptpPrettify(a) + " + " + tptpPrettify(b) + " = " + tptpPrettify(c)
      case AppRec(Sym("add"), Seq(a, b)) => tptpPrettify(a) + " + " + tptpPrettify(b)
      case AppRec(Sym("product"), Seq(a, b, c)) => tptpPrettify(a) + " * " + tptpPrettify(b) + " = " + tptpPrettify(c)
      case AppRec(Sym("multiply"), Seq(a, b)) => tptpPrettify(a) + " * " + tptpPrettify(b)
      case x => x.toString
    }
  }

  def tptpPrettify(clause: Clause): String = {
    val (ant, suc) = clause.map(tptpPrettify, tptpPrettify)
    ant.map("¬" + _).mkString(" ∨ ") + " ⊢ " + suc.mkString(" ∨ ")
  }
}
