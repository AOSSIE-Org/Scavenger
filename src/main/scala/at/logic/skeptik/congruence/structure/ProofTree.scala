package at.logic.skeptik.congruence.structure

import at.logic.skeptik.expression._
import at.logic.skeptik.algorithm.dijkstra._
import scala.collection.mutable.{ListBuffer, StringBuilder, HashMap => MMap}
import scala.collection.immutable.Queue

case class ProofForest(
    next: Map[E,(E,Option[EqW])] = Map[E,(E,Option[EqW])](), 
    rootSize: Map[E,Int] = Map[E,Int](), 
    edges: Map[(E,E),Option[EqW]] = Map[(E,E),Option[EqW]](),
    pForder: Queue[(E,E)] = Queue[(E,E)]()) 
      extends CongruenceGraph(edges,pForder) {
  
  def newGraph(edges: Map[(E,E),Option[EqW]], order: Queue[(E,E)]): CongruenceGraph = {
    ProofForest(next,rootSize,edges,order)
  }
  
  def addEdge(u: E, v: E, eq: Option[EqW]) = {
//    println("before: " + this)
//    println("adding: " + (u,v,eq))
    val uR = root(u)
    val vR = root(v)
    val res = if (uR != vR) {
      val uIn = if (!rootSize.isDefinedAt(uR)) ProofForest(next,rootSize + (u -> 1),edges,order) else this
      val vIn = if (!rootSize.isDefinedAt(v)) ProofForest(uIn.next,uIn.rootSize +(v -> 1),edges,order) else uIn
      if (vIn.rootSize(uR) > vIn.rootSize(vR)) {
        vIn.insertEdge(u,uR,v,vR,eq)
      }
      else {
        vIn.insertEdge(v,vR,u,uR,eq)
      }
    }
    else this
//    println("result:" + res)
    res
  }
  
  def root(u: E) = {
    var node = u
    while (next.isDefinedAt(node)) {
      node = next(node)._1
    }
    node
  }
  
  def reversePathList(orig: List[(E,Option[EqW],E)]) = {
    orig.foldLeft(List[(E,Option[EqW],E)]())({(A,B) => 
      A.+:((B._3,B._2,B._1))
    })
  }
  
  def ncaPath(u: E, v: E) = {
    val p1 = rootPath(u)
    val p2 = rootPath(v)
//    println("node / root: " + (u,p1)  + " and " + (v,p2))
    if (p1.lastOption.getOrElse((u,None,u))._3 == p2.lastOption.getOrElse((v,None,v))._3) {
      val path = p1.diff(p2) ++ reversePathList(p2.diff(p1))
//      if (root(u) != root(v) && !path.isEmpty) println("building path for non congruent terms: " + (u,v))
      path
    }
    else List()
    
  }
  
  /**
   * Let c be the nearest common ancestor of u and v in the proof tree
   * The explanation in form of an EquationPath is found by traversing the path from u to c concatinated with the path from c to v.
   * In each step of the path, if an equation is set as edge label, it is original and no deduce paths have to be created.
   * If no equation is set, then the equality has to be deduced and paths for the two arguments are created
   */
  def explain(u: E, v: E)(implicit eqReferences: MMap[(E,E),EqW]): Option[EquationPath] = {
//    println("ORDER SIZE: " + order.size +" vs " + edges.size + " ~ \n"+order + "\n"+edges)
    var ord = order
    var graph = this
    while (!ord.isEmpty) {
      val (currEl,currOrd) = ord.dequeue
      ord = currOrd
//      if (!graph.edges.isDefinedAt(currEl)) {
//        println(graph.edges + "\n" + ord)
//      }
      graph = ProofForest(graph.next,graph.rootSize,edges,ord).asInstanceOf[ProofForest]
      graph = graph.addEdge(currEl._1, currEl._2, graph.edges.getOrElse((currEl),graph.edges(currEl.swap)))
    }
//    val realTree = lazyEdges.foldLeft(this)({(A,B) => 
//      A.addEdge(B._1._1, B._1._2, B._2)
//    })
    val path = graph.ncaPath(u,v) 
    if (path.isEmpty) {
      if (u == v) {
        val end = new EquationPath(u,None)
        val x = EqW(u,v)
        val eqTreeEdge = new EqTreeEdge(end,EqLabel(x,Set[EquationPath]()))
        val y = new EquationPath(u,Some(eqTreeEdge))
        Some(y)
      }
      else {
//        println("no explanation for " + (u,v))
        None
      }
    }
    else {
      val x = graph.explainAlongPath(path)
      
      if (!(((x.firstVert == u) && (x.lastVert == v)) || ((x.firstVert == v) && (x.lastVert == u)))){
//        println("faulty expl for " + (u,v) + "\n"+path)
      }
      Some(x)
    }
  }
    
  def explainAlongPath(path: List[(E,Option[EqW],E)])(implicit eqReferences: MMap[(E,E),EqW]): EquationPath = {
//    println(path)
    val (t1,eq,t2) = path.head
    var end = false
    val realEq = eq.getOrElse({
      val x = EqW(t1,t2,false) //Probably causing bugs!
//      if (x.toString == "((f1 c_1) = (f1 (f1 c_2 c_3)))") println("creating ((f1 c_1) = (f1 (f1 c_2 c_3))) in explainAlongPath")
//      if (x.toString == "((f1 c_1) = (f1 (f1 c_2 c_3)))") println("creating ((f1 c_1) = (f1 (f1 c_2 c_3))) in explainAlongPath")
      x
    })
//    if (!eq.isDefined) println("EQ NOT DEFINED IN EXPLAIN OF PROOFTREE")
    val deduceTrees = buildDD(t1,eq,t2)
//    println(eq + " real eq: " + realEq)
    val eqL = EqLabel(realEq,deduceTrees)
    val nextEdge = if (path.size > 1)
      explainAlongPath(path.tail)
    else {
//      println(path + " ending!")
      end = true
      val x = new EquationPath(t2,None)
//      println(x)
      x
    }
    val eqEdge = EqTreeEdge(nextEdge,eqL)
    val y = new EquationPath(t1,Some(eqEdge))
//    if (end) println(y)
    y
  }
  
  def rootPath(u: E) = {
    val path = ListBuffer[(E,Option[EqW],E)]()
    var node = u
    while (next.isDefinedAt(node)) {
      val nn = next(node)
      path.+=((node,nn._2,nn._1))
      node = nn._1
    }
    path.toList
  }
  
  private def insertEdge(u: E, uRoot: E, v: E, vRoot: E, eq: Option[EqW]): ProofForest = {
//    println("adding " + v + " to " + u)
    val reversed = reverseToRoot(v)
    val finalSize = reversed.rootSize.updated(uRoot,reversed.rootSize(uRoot) + reversed.rootSize(vRoot)) - v
    val finalNext = reversed.next.updated(v, (u,eq))
    ProofForest(finalNext,finalSize,edges - ((u,v)) - ((v,u)),order) //probably don't need to delete (v,u), because it should not be in the map
  }
  
  def reverseToRoot(u: E): ProofForest = {
//    println("reversing " + u)
    val path = rootPath(u)
    val revNext = path.foldLeft(next)({(A,B) =>
      val (node1,eq,node2) = B
      A.updated(node2,(node1,eq))
    })
    val finalNext = revNext - u
    ProofForest(finalNext,rootSize,edges,order)
  }
  
  def printNode(u: E) = {
    var node = u
    var out = new StringBuilder
    while (next.isDefinedAt(node)) {
      out.append(node +" -> ")
      node = next(node)._1
    }
    out.append(node.toString)
  }
}