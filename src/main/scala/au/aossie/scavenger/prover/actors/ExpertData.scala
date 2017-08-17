package au.aossie.scavenger.prover.actors

import akka.actor.ActorSystem
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression.{AppRec, E, Sym, Var}
import au.aossie.scavenger.proof.cr._
import au.aossie.scavenger.structure.immutable.{Clause, Literal}
import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.choosing.DecisionsMaker
import au.aossie.scavenger.unification.{Unificator, MartelliMontanari => unify}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class ExpertData(predicates: Set[Sym], withSetOfSupport: Boolean, maxIterationsWithoutDecision: Int)(implicit rnd: Random, implicit val system: ActorSystem) {
  val clauses: ListBuffer[ClauseInfo] = ListBuffer.empty
  val indexByClause: mutable.Map[Clause, Int] = mutable.Map.empty
  val propagatedClauses: mutable.Set[(Clause, Clause)] = mutable.Set.empty
  val provedLiterals: mutable.Map[Literal, Int] = mutable.Map.empty
  val lastPropagatedLiterals: mutable.ListBuffer[(Literal, CRProofNode)] = mutable.ListBuffer.empty
  val unificator: Unificator = new Unificator()
  val decisionsMaker = new DecisionsMaker(maxIterationsWithoutDecision)

  def addNewClause(crProofNode: CRProofNode,
                   expertClause: Clause): Unit = {
    val globalClause = (expertClause, crProofNode.nonExpertDecisions.toClause)
    if (!propagatedClauses.contains(globalClause)) {
      propagatedClauses.add(globalClause)
      if (expertClause.isUnit) {
        lastPropagatedLiterals.append((expertClause.literal, crProofNode))
      }

      val index = indexByClause.getOrElseUpdate(expertClause, clauses.size)
      if (index == clauses.size) {
        clauses += ClauseInfo(expertClause, ListBuffer(crProofNode))

        if (expertClause.isUnit) {
          unificator.addB(expertClause.literal)
          provedLiterals.update(expertClause.literal, clauses.size - 1)
        } else {
          expertClause.literals.foreach(unificator.addA)
        }
      } else {
        clauses(index).proofs.append(crProofNode)
      }
    }
  }

  def addInitialClauses(newClauses: Seq[Clause]): Unit = {
    newClauses.foreach { clause =>
      val initialStatement = new InitialStatement(clause)
      val node = Expertise(initialStatement, predicates)
      if (node.conclusion != Clause.empty) {
        addNewClause(node, node.conclusion)
      }
    }
  }

  def addClauses(nodes: Seq[CRProofNode]): Unit = {
    nodes.foreach { globalNode =>
      val localNode = Expertise(globalNode, predicates)
      if (localNode.conclusion != Clause.empty) {
        addNewClause(localNode, localNode.conclusion)
      }
    }
  }

  def resolveUnitPropagation(): Unit = {
    clauses.collect { case ClauseInfo(clause, proofs) if proofs.nonEmpty => if (!clause.isUnit)
      for (clauseNode <- rnd.shuffle(proofs)) {
        // TODO: Think about to shuffle literals to avoid worst case in the bruteforce.
        val shuffledLiterals = clause.literals

        val unifyCandidates = shuffledLiterals.map(literal => rnd.shuffle(unificator.getUnifications(literal)))
        val emptyCandidates = unifyCandidates.indices.filter(unifyCandidates(_).isEmpty)
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
              for (unifierNodes <- combinations(chosenUnifiers.map(l => clauses(indexByClause(l.toClause)).proofs))) {

                if (!withSetOfSupport || unifierNodes.exists(!_.isAxiom) || !clauseNode.isAxiom) {
                  // FIXME: Why this is never used?
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
                  addNewClause(unitPropagationNode, newLiteral)
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
              for (curUni <- candidates) if (clauses(indexByClause(curUni.toClause)).proofs.nonEmpty) {
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
    val cdclNodes: mutable.ListBuffer[CRProofNode] = mutable.ListBuffer.empty
    val wasCDCL: mutable.Set[Clause] = mutable.Set.empty
    for ((literal, conflictNode) <- lastPropagatedLiterals) {
      val bucketName = getBucketByExpr(literal.unit)

      val candidateLiterals = provedLiteralBuckets.getOrElse(bucketName, ListBuffer.empty[Literal])

      for {
        otherLiteral <- candidateLiterals if (literal.polarity != otherLiteral.polarity) && unifyWithRename(Seq(literal.unit), Seq(otherLiteral.unit)).isDefined
        otherNode <- clauses(indexByClause(otherLiteral.toClause)).proofs
        conflict = Conflict(conflictNode, otherNode)
      } {
        val cdclNode = ConflictDrivenClauseLearning(conflict)
        if (!wasCDCL.contains(cdclNode.conclusion)) {
          wasCDCL.add(cdclNode.conclusion)
          cdclNodes += cdclNode
          if (cdclNode.conclusion == Clause.empty) {
            return cdclNodes
          }
        }
      }
      provedLiteralBuckets.getOrElseUpdate(bucketName, ListBuffer.empty).append(literal)
    }


    lastPropagatedLiterals.clear()

    cdclNodes
  }

  def removeCDCLPremises(premises: Set[Literal]): Unit = {
    provedLiterals.foreach {
      case (_, index) =>
        val nonValidProofs = clauses(index).proofs.filter {
          case ConflictDrivenClauseLearning(conflict) =>
            conflict.decisions.exists(premises.contains)
          case node =>
            node.decisions.exists(premises.contains)
        }
        clauses(index).proofs --= nonValidProofs
    }

    val nonValidLiterals = provedLiterals.filter { case (_, index) => clauses(index).proofs.isEmpty }.keys
    provedLiterals --= nonValidLiterals
  }

  def makeDecision(wasNewCDCLs: Boolean): Unit = {
    decisionsMaker.incCounter
    if (!wasNewCDCLs || decisionsMaker.counterExpired) {
      val available = clauses
        .filterNot(clauseInfo =>
          clauseInfo.proofs.isEmpty || clauseInfo.expertClause.literals.exists(provedLiterals.contains))
        .flatMap(_.expertClause.literals)
      if (available.nonEmpty) {
        val newDecision = decisionsMaker.makeDecision(available)
        addNewClause(Decision(newDecision), newDecision)
      } else {

      }
    }

  }
}

case class ClauseInfo(expertClause: Clause,
                      proofs: ListBuffer[CRProofNode]) /// proofNodes and the list of decisions for non-expert part of the clause
