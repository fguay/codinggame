import math._
import scala.util._



/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player4 extends App {
  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, e) = for(i <- readLine split " ") yield i.toInt

  Console.err.println(s"$n $l $e")

  val allLinks = (0 until l).map{ _ => (readLine split " ").map{_.toInt}}.map{ a => (a(0), a(1))}
  val gateways = (0 until e).map(_ => readInt)
  logger(s"All links $allLinks")
  logger(s"Gateway : $gateways")
  loop(allLinks)


  trait Node {
    def position:Int
    def equals(otherNode:Node): Boolean = position.equals(otherNode.position)
    def equals(otherPosition:Int) : Boolean = position.equals(otherPosition)
  }

  case class Tree(val position:Int, val childs:Seq[Node]) extends Node
  case class Gateway(val position:Int) extends Node
  case class Edge(val position:Int) extends Node
  case class Path(val nodes:Seq[Node]) {
    def :+ (node:Node) = Path(nodes :+ node)
  }


  case class Link (val parent:Node, val child:Node) {
    def contains(position:Int) : Boolean = parent.equals(position) || child.equals(position)
    def contains(node:Node) : Boolean = parent.equals(node) || child.equals(node)
    def print(): String = s"${parent.position} ${child.position}"
    def tuple()=(parent.position,child.position)
    def removeFrom(availableLinks:Seq[(Int,Int)]) = availableLinks.filterNot(l => contains(l._1) && contains(l._2))
    def toGateway:Boolean = child.isInstanceOf[Gateway]
    def other(node: Node) = if(parent.equals(node)) child else parent
  }


  object GraphBuilder {

    def buildNode(availableLinks:Seq[(Int,Int)]):Seq[Node]= {
      availableLinks.flatMap(l => Seq(l._1, l._2)).distinct.map{ position =>
        if(gateways.contains(position)) Gateway(position) else Edge(position)
      }
    }


    def buildLinks(availableLinks:Seq[(Int,Int)], nodes:Seq[Node]): Seq[Link] = {
      availableLinks.map { l=>
      {
        (nodes.filter(n => n.equals(l._1)).head,nodes.filter(n => n.equals(l._2)).head) match {
          case (e:Edge, g:Gateway) => Link(e,g)
          case (g:Gateway, e:Edge) => Link(e,g)
          case (e1, e2) => Link(e1,e2)
        }
      }
      }
    }

    def buildPath(position:Node, links:Seq[Link]) : Seq[Path] = {
      logger(s"Path for ${position}")
      logger(s"availabe links : ${links}")
      val subLinks = links.filter(l => l.contains(position) )
      val subNodes = subLinks.map(l => l.other(position) )
      if(subNodes.isEmpty){
        Seq(Path(Seq(position)))
      } else {
        subNodes.map{ n => buildPath(n, subLinks)}.map{s => s.map{ s => s :+ position } }.flatten
      }
    }

    def buildTree(position:Int, availableLinks:Seq[(Int,Int)]) : Node = {
      logger(s"Tree for ${position}")
      logger(s"availabe links : ${availableLinks}")
      val nodes = buildNode(availableLinks)
      logger(s"availabe node : ${nodes}")
      val root = nodes.filter(n => n.equals(position)).head
      logger(s"root : ${root}")
      root match {
        case g:Gateway => g
        case e:Edge => {
          val links = buildLinks(availableLinks, nodes)
          val subLinks = links.filter(l => l.contains(root) )
          val nextLinks = links.diff(subLinks).map(l => l.tuple())
          logger(s"links : ${nextLinks}")
          if(nextLinks.isEmpty){
            root
          } else {
            val subTrees = subLinks.map(l => buildTree(l.other(root).position, nextLinks) )
            Tree(root.position, subTrees)
          }
        }
      }
    }
  }


  def loop(availlableLinks:Seq[(Int,Int)]): Unit = {
    val si = readInt // The index of the node on which the Skynet agent is positioned this turn
    logger(s"Skynet position $si")
    val nodes = GraphBuilder.buildNode(availlableLinks)
    val links = GraphBuilder.buildLinks(availlableLinks, nodes)
    val root = nodes.filter(n => n.equals(si)).head
    val graph = GraphBuilder.buildPath(root, links)
    logger(s"Graph : ${graph.toString}")

    //val link = GraphEvaluator.betterLink(graph)
    //println(link.print())
    //loop(link.removeFrom(availlableLinks))

  }

  def logger(s:String) = Console.err.println(s"$s")

}