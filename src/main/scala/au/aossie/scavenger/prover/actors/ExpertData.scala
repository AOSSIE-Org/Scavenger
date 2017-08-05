package au.aossie.scavenger.prover.actors

import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression.{AppRec, E, Sym, Var}
import au.aossie.scavenger.proof.cr.{CRProofNode, InitialStatement, UnitPropagationResolution}
import au.aossie.scavenger.structure.immutable.{AxiomClause, Clause, Literal}
import au.aossie.scavenger.prover._
import au.aossie.scavenger.unification.MartelliMontanari

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class ExpertData(predicates: Set[Sym])(implicit rnd: Random) {
  val clauses: ListBuffer[ClauseInfo] = ListBuffer.empty
  val indexByClause: mutable.Map[Clause, Int] = mutable.Map.empty

  def addNewClause(newClause: ClauseInfo): Unit = {
    val index = indexByClause.getOrElseUpdate(newClause.expertClause, clauses.size)
    if (index == clauses.size) {
      clauses += newClause
    } else {
      clauses(index).nonExpertClauses ++= newClause.nonExpertClauses
    }
  }

  def addNewClauses(newClauses: Seq[Clause]): Unit = {
    newClauses.foreach { clause =>
      val (expertLiterals, nonExpertLiterals) = clause.literals.partition {
        case AppRec(p: Sym, _) =>
          predicates.contains(p)
      }
      if (expertLiterals.nonEmpty) {
        addNewClause(ClauseInfo(
          expertLiterals.toClause,
          ListBuffer(InitialStatement(expertLiterals.toClause)),
          ListBuffer(nonExpertLiterals.toClause)
        ))
      }
    }
  }

  def resolveUnitPropagation: Unit = {
    val propagatedLiterals = mutable.Set.empty[Literal]
    clauses.foreach { case ClauseInfo(clause, proofNodes, _) =>
      for (clauseNode <- proofNodes) {
        // TODO: Think about to shuffle literals to avoid worst case in the bruteforce.
        val shuffledLiterals = clause.literals

        val unifyCandidatesIndexes = shuffledLiterals.map { literal =>
          val id = rnd.nextInt(indexesByClause(literal).size)
          indexesByClause(literal)(id)
        }

        for (conclusionId <- unifyCandidatesIndexes.indices) {
          // TODO: Think about other cut offs.
          // TODO: it could be done in linear time.
          if (unifyCandidatesIndexes.indices.forall(id => id == conclusionId || unifyCandidatesIndexes(id).nonEmpty)) {
            val unifiers: Seq[Seq[Int]] = unifyCandidatesIndexes.take(conclusionId) ++ unifyCandidatesIndexes.drop(conclusionId + 1)
              val literals: Seq[Literal] = shuffledLiterals.take(conclusionId) ++ shuffledLiterals.drop(conclusionId + 1)

              def newPropagation(chosenUnifiers: mutable.Seq[Literal],
                                 subs: mutable.Seq[Substitution],
                                 globalSubst: Substitution,
                                 usedVars: mutable.Set[Var]): Unit = {
                val unifierNodes = chosenUnifiers.map(l => rnd.shuffle(bufferNodes(reverseImplication(l.toClause))).head)
                if (!withSetOfSupport || unifierNodes.exists(!_.isAxiom) || !clauseNode.isAxiom) {
                  val curSubst = renameVars(shuffledLiterals(conclusionId).unit, usedVars)
                  val unitPropagationNode =
                    UnitPropagationResolution(
                      unifierNodes,
                      clauseNode,
                      shuffledLiterals(conclusionId),
                      literals,
                      subs,
                      globalSubst
                    )
                  val newLiteral = unitPropagationNode.conclusion.literal
                  if (!result.contains(newLiteral) && !provedLiterals.contains(newLiteral)) {
                    addNode(newLiteral.toClause, unitPropagationNode)
                    result += newLiteral
                  }
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
  }
}

case class ClauseInfo(expertClause: Clause,
                      proofNodes: ListBuffer[CRProofNode],
                      nonExpertClauses: ListBuffer[Clause])
