package au.aossie.scavenger.exporter

import au.aossie.scavenger.expression.E
import au.aossie.scavenger.judgment.Sequent
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.sequent.{SequentProofNode => N}

import java.io.Writer

abstract class Exporter extends Writer {
  def write(e: E)
  def write(s: Sequent)
  def write(p: Proof[N])
}
