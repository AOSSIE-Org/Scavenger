package au.aossie.scavenger.parser

import scala.util.parsing.combinator._

import java.io.FileReader
import au.aossie.scavenger.proof.{Proof,ProofNode}
import au.aossie.scavenger.structure.Sequent

abstract class ProofCombinatorParser[N <: ProofNode[Sequent,N]] extends ProofParser[N] with RegexParsers {
  def proof : Parser[Proof[N]]
  
  def reset() : Unit // resets any internal state of the parser
  
  def read(filename: String) : Proof[N] = {
    val p = parse(proof, new FileReader(filename)) match {
      case Success(p,_) => p
      case Failure(message,_) => throw new Exception("Failure: " + message)
      case Error(message,_) => throw new Exception("Error: " + message)
    }
    reset()
    p
  }
}

