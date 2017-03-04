package au.aossie.scavenger.exporter

import au.aossie.scavenger.expression.E
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.cr.{CRProofNode => N}
import java.io.Writer

import au.aossie.scavenger.structure.immutable.Clause

abstract class Exporter extends Writer {
  def write(e: E)
  def write(s: Clause)
  def write(p: Proof[N])
}

