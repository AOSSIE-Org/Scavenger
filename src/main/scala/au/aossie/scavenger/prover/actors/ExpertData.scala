package au.aossie.scavenger.prover.actors

import akka.actor.{ActorSystem, Props}
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression.{AppRec, E, Sym, Var}
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.cr._
import au.aossie.scavenger.structure.immutable.{Clause, Literal}
import au.aossie.scavenger.prover._
import au.aossie.scavenger.unification.{Unificator, MartelliMontanari => unify}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class ExpertData(predicates: Set[Sym], withSetOfSupport: Boolean)(implicit rnd: Random, implicit val system: ActorSystem) {
  val clauses: ListBuffer[ClauseInfo] = ListBuffer.empty
  val indexByClause: mutable.Map[Clause, Int] = mutable.Map.empty
  val provedLiterals: mutable.Map[Literal, Int] = mutable.Map.empty
  val lastPropagatedLiterals: mutable.ListBuffer[(Literal, CRProofNode)] = mutable.ListBuffer.empty
  val unificator: Unificator = new Unificator()

  def addNewClause(crProofNode: CRProofNode,
                   expertClause: Clause,
                   nonExpertClause: Clause): Unit = {
    val index = indexByClause.getOrElseUpdate(expertClause, clauses.size)
    if (index == clauses.size) {
      clauses += ClauseInfo(expertClause, ListBuffer((crProofNode, nonExpertClause)))

      if (expertClause.isUnit) {
        unificator.addB(expertClause.literal)
        provedLiterals.update(expertClause.literal, clauses.size - 1)
      } else {
        expertClause.literals.foreach(unificator.addA)
      }
    } else {
      clauses(index).proofs.append((crProofNode, nonExpertClause))
    }
  }

  def addInitialClauses(newClauses: Seq[Clause]): Unit = {
    newClauses.foreach { clause =>
      val (expertLiterals, nonExpertLiterals) = clause.literals.partition {
        case AppRec(p: Sym, _) =>
          predicates.contains(p)
      }
      if (expertLiterals.nonEmpty) {
        addNewClause(InitialStatement(expertLiterals.toClause), expertLiterals.toClause, nonExpertLiterals.toClause)
      }
    }
  }

  def resolveUnitPropagation: Unit = {
    clauses.foreach { case ClauseInfo(clause, proofs) => // TODO: if !clause.isUnit
      for ((clauseNode, _) <- proofs) {
        // TODO: Think about to shuffle literals to avoid worst case in the bruteforce.
        val shuffledLiterals = clause.literals

        val unifyCandidates = shuffledLiterals.map(literal => unificator.getUnifications(literal))
        val emptyCandidates = unifyCandidates.indices.filterNot(unifyCandidates(_).isEmpty)
        if (emptyCandidates.size < 2) {
          val candidateIndices = if (emptyCandidates.isEmpty) unifyCandidates.indices else emptyCandidates
          for (conclusionId <- candidateIndices) {
            val unifiers: Seq[Seq[Literal]] = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
            val literals: Seq[Literal] = shuffledLiterals.take(conclusionId) ++ shuffledLiterals.drop(conclusionId + 1)

            def newPropagation(chosenUnifiers: mutable.Seq[Literal],
                               subs: mutable.Seq[Substitution],
                               globalSubst: Substitution,
                               usedVars: mutable.Set[Var]): Unit = {
              // TODO: Or is it better to iterate over all possible proofNodes?
              val unifierNodes = chosenUnifiers.map {
                l =>
                  val proofNodes = clauses(indexByClause(l.toClause)).proofs
                  val randomIndex = rnd.nextInt(proofNodes.size)
                  proofNodes(randomIndex)
              }

              if (!withSetOfSupport || unifierNodes.exists(!_._1.isAxiom) || !clauseNode.isAxiom) {
                // TODO: Why this is never used?
                val curSubst = renameVars(shuffledLiterals(conclusionId).unit, usedVars)
                val unitPropagationNode =
                  UnitPropagationResolution(
                    unifierNodes.map(_._1),
                    clauseNode,
                    shuffledLiterals(conclusionId), // TODO: maybe we should apply curSubst???
                    literals,
                    subs,
                    globalSubst
                  )
                val newLiteral = unitPropagationNode.conclusion.literal
                if (!provedLiterals.contains(newLiteral)) {
                  unificator.addB(newLiteral)
                }
//                provedLiterals.getOrElseUpdate(newLiteral, mutable.ListBuffer.empty).append(unitPropagationNode)
                lastPropagatedLiterals.append((newLiteral, unitPropagationNode))
              }
            }

            /**
              * Recursively generates UnitPropagationResolution and verifies unification on each step.
              *
              * @param chosenUnifiers    chosen literals
              * @param subs              substitutions for literals to ensure that variables in different literals are different
              * @param literalsWithSubst literals with unique variables
              * @param globalSubst       same global MGU for all literals
              * @param cur               number of chosen literals
              */
            def go(chosenUnifiers: mutable.Seq[Literal],
                   unifierWithSub: Seq[E],
                   subs: mutable.Seq[Substitution],
                   literalsWithSubst: Seq[Literal],
                   globalSubst: Substitution,
                   usedVars: mutable.Set[Var],
                   cur: Int): Unit = {

              if (cur == unifiers.size) {
                newPropagation(chosenUnifiers, subs, globalSubst, usedVars)
                return
              }

              // NOTE: Looking at all possible unifications turns to large complexity of this part of resolving
              // TODO: Think about to check only random K unifiers
              val candidates = {
                //                rnd.shuffle(unifiers(cur)).take(maxCountCandidates)
                unifiers(cur)
              }
              for (curUni <- candidates) {
                val substitution = renameVars(curUni.unit, usedVars)
                val newSubs = subs :+ substitution(globalSubst)
                val leftWithSubst = globalSubst(substitution(curUni.unit))
                val newUnifierWithSubst = unifierWithSub :+ leftWithSubst

                // NOTE: Very dangerous to call pure unify method
                val unificationSubst = unify(leftWithSubst, literalsWithSubst(cur).unit)
                unificationSubst match {
                  case Some(uniSubst) =>
                    go(chosenUnifiers :+ curUni,
                      newUnifierWithSubst.map(uniSubst(_)),
                      newSubs.map(_ (uniSubst)),
                      literalsWithSubst.map(uniSubst(_)),
                      globalSubst(uniSubst),
                      usedVars ++ leftWithSubst.variables,
                      cur + 1
                    )
                  case None =>
                }
              }
            }

            go(mutable.Seq.empty,
              Seq.empty,
              mutable.Seq.empty,
              literals,
              Substitution.empty,
              mutable.Set[Var](literals.map(_.unit.variables).reduce {
                _ ++ _
              }: _*),
              0)
          }
        }
      }
    }
  }

  def getBucketByExpr(expr: E): String = expr match {
    case AppRec(Sym(name), _) =>
      name
  }

  def resolveCDCL: Seq[CRProofNode] = {
    val provedLiteralBuckets: mutable.Map[String, ListBuffer[Literal]] = mutable.Map.empty
    val cdclClauses: mutable.ListBuffer[CRProofNode] = mutable.ListBuffer.empty
    for ((literal, conflictNode) <- lastPropagatedLiterals) {
      val bucketName = getBucketByExpr(literal.unit)

      val candidateLiterals = provedLiteralBuckets.getOrElse(bucketName, ListBuffer.empty[Literal])

      for {
        otherLiteral <- candidateLiterals if (literal.polarity != otherLiteral.polarity) && unifyWithRename(Seq(literal.unit), Seq(otherLiteral.unit)).isDefined
        (otherNode, nonExpertClause) <- clauses(indexByClause(otherLiteral.toClause)).proofs
        conflict = Conflict(conflictNode, otherNode)
      } {
        val cdclNode = ConflictDrivenClauseLearning(conflict)
        cdclClauses += cdclNode
        if (cdclNode.conclusion == Clause.empty) {
          return cdclClauses
        }
      }
      provedLiteralBuckets.getOrElseUpdate(bucketName, ListBuffer.empty).append(literal)
    }


    lastPropagatedLiterals.clear()

    cdclClauses
  }
}

case class ClauseInfo(expertClause: Clause,
                      proofs: ListBuffer[(CRProofNode, Clause)]) /// proofNodes and the list of decisions for non-expert part of the clause
