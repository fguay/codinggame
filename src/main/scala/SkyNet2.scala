import math._
import scala.util._



/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, e) = for(i <- readLine split " ") yield i.toInt

  Console.err.println(s"$n $l $e")

  trait Node  {
     val distance:Int = parent.map(p => p.distance + 1).getOrElse(0)
     val parent:Option[Node]
     val position:Int
     val nextGatewayLinks:Option[Seq[Node]]
     val childLinks:Option[Seq[Node]]
     val nbGateway:Int
     override def toString():String = s"${parent.map {_.position}.getOrElse("")} $position"
     def log():String
     def getBetterGateway():Gateway
     def equalsLinks(link:(Int,Int)) = parent match {
       case Some(p) => ((p.position, position) == link || (position, p.position) == link)
       case _ =>  false
     }
     def filterLinks(availableLinks: Seq[(Int,Int)]) = availableLinks.filterNot(l => equalsLinks(l))
  }

  case class Path(path: Seq[Node])


  class Edge(val parent:Option[Node], val position:Int,  availableLinks: Seq[(Int,Int)]) extends Node {

    override val childLinks:Option[Seq[Node]] = if(distance > 3) None else getChilds

    val nextGatewayLinks:Option[Seq[Gateway]] = childLinks match {
      case Some(childs) => childs.filter(p => p.isInstanceOf[Gateway]).map(p => p.asInstanceOf[Gateway]) match {
        case Seq() => None
        case s => Some(s)
      }
      case _ => None
    }

    val nbGateway: Int = childLinks match {
      case Some(childs) => childs.map { n => n.nbGateway}.sum
      case None => 0
    }

    def getChilds:Option[Seq[Node]] = {
      val linksWithoutMe = filterLinks(availableLinks)
      val nodeLinks = linksWithoutMe.filter(c => (c._1 == position || c._2 == position))
      val childs:Option[Seq[Node]] = nodeLinks match {
        case Seq() => None
        case nodes =>  Some(nodes.map{ c =>
          if(gateways.contains(c._1)) new Gateway(this , c._1, linksWithoutMe)
          else if(gateways.contains(c._2)) new Gateway(this , c._2, linksWithoutMe)
          else if (c._1 == position) new Edge(Some(this), c._2, linksWithoutMe)
          else new Edge(Some( this ), c._1, linksWithoutMe)})
      }

      logger("Find childs for node : " + position + " and distance " + distance + " available links :" + availableLinks.size + " childs : " + childs)
      childs
    }


    def log():String=childLinks match {
      case Some(childs) => childs.map(n => s"$position -> ${n.log()}").mkString("\n")
      case None => "No children ?? "
    }
    def getBetterGateway():Gateway = (childLinks, nextGatewayLinks) match {
      case (Some(childs), None) => childs.sortBy(_.nbGateway).reverse.head.getBetterGateway()
      case (Some(childs), Some(nextGat)) if nextGat.size > 1 => nextGat.head
    }
  }

  class Root( position:Int,  availableLinks: Seq[(Int,Int)]) extends Edge(None, position, availableLinks) {

    override def log():String=childLinks match {
      case Some(childs) => childs.map(n => s">> $position -> ${n.log()}").mkString("\n")
      case None => "No children ?? "
    }
    override def getBetterGateway():Gateway = nextGatewayLinks match {
      case Some(gats) => gats.head
      case None => super.getBetterGateway()
    }
  }

  class Gateway ( parent:Node,  position:Int,  availableLinks: Seq[(Int,Int)]) extends Edge(Some(parent), position, availableLinks) {
    override def getChilds:Option[Seq[Node]] = None
    override val nbGateway = 1
    override def log():String=s"||$position||"
    override def getBetterGateway():Gateway = this
  }


  val allLinks = (0 until l).map{ _ => (readLine split " ").map{_.toInt}}.map{ a => (a(0), a(1))}
  val gateways = (0 until e).map(_ => readInt)
  logger(s"All links $allLinks")
  logger(s"Gateway : $gateways")
  loop(allLinks)



  def loop(availlableLinks:Seq[(Int,Int)]): Unit = {
    val si = readInt // The index of the node on which the Skynet agent is positioned this turn
    logger(s"Skynet position $si")
    val graph = new Root(si, availlableLinks)
    logger(graph.log())
    val gateway = graph.getBetterGateway()
    println(gateway)
    loop(gateway.filterLinks(availlableLinks))

  }

  def logger(s:String) = Console.err.println(s"$s")

}