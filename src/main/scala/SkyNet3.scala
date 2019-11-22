import math._
import scala.util._



/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player3 extends App {
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

  case class Link (val parent:Node, val child:Node) {
    def contains(position:Int) : Boolean = parent.equals(position) || child.equals(position)
    def contains(node:Node) : Boolean = parent.equals(node) || child.equals(node)
    def print(): String = s"${parent.position} ${child.position}"
    def removeFrom(availableLinks:Seq[(Int,Int)]) = availableLinks.filterNot(l => contains(l._1) && contains(l._2))
    def toGateway:Boolean = child.isInstanceOf[Gateway]
  }

  case class Edge(val position:Int) extends Node
  case class Gateway(val position:Int) extends Node

  case class Graph(position:Int, links: Seq[Link]) extends Node


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

    def buildGraph(position:Int, availableLinks:Seq[(Int,Int)]) : Graph = {
      logger(s"availabe links : ${availableLinks}")
      val nodes = buildNode(availableLinks)
      logger(s"availabe node : ${nodes}")
      val root = nodes.filter(n => n.equals(position)).head
      logger(s"root : ${root}")
      Graph(root.position, buildLinks(availableLinks, nodes))
    }
  }

  object GraphEvaluator {
    def nextLink(graph: Graph): Seq[Link] = graph.links.filter(l => l.contains(graph.position))
    def nextGateway(graph: Graph): Seq[Link] = nextLink(graph).filter(l => l.toGateway)
    def nextGateways(graph: Graph): Seq[Link] = graph.links.filter(l => l.toGateway)

    def betterLink(graph: Graph) : Link = {
      val gatewayLink = nextGateway(graph)
      if(!gatewayLink.isEmpty) {
        gatewayLink.head // hope there are only one...
      } else {
        val sortedLink = nextGateways(graph).groupBy(_.parent).toSeq.groupBy(_._2.size).toSeq.sortBy(_._1).reverse
        logger(s"sorted link : ${sortedLink}")
        val links = sortedLink.head._2
        logger(s"Priority link : $links")
        links.head._2.head
      }
    }
  }


  def loop(availlableLinks:Seq[(Int,Int)]): Unit = {
    val si = readInt // The index of the node on which the Skynet agent is positioned this turn
    logger(s"Skynet position $si")
    val graph = GraphBuilder.buildGraph(si, availlableLinks)
    logger(s"Graph : ${graph.toString}")

    val link = GraphEvaluator.betterLink(graph)
    println(link.print())
    loop(link.removeFrom(availlableLinks))

  }

  def logger(s:String) = Console.err.println(s"$s")

}