package org.aossie.scavenger.exporter

import org.aossie.scavenger.expression.E
import org.aossie.scavenger.proof.Proof
import org.aossie.scavenger.proof.cr.{CRProofNode => N}

import org.aossie.scavenger.structure.immutable.Clause
import org.aossie.scavenger.util.io.Output

abstract class Exporter extends Output {
  def write(e: E)
  def write(s: Clause)
  def write(p: Proof[N])
}

