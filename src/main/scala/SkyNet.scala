import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object SkyNet extends App {
  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, e) = for(i <- readLine split " ") yield i.toInt

  Console.err.println(s"$n $l $e")

  val allLinks = (0 until l).map{ _ => (readLine split " ").map{_.toInt}}.map{ a => (a(0), a(1))}
  val gateways = (0 until e).map(_ => readInt)
  log(s"All links $allLinks")
  log(s"Gateway : $gateways")
  loop(allLinks)


  def nextPositionIsGateway(position: Seq[Int], gatewayLink: Seq[(Int, Int)]) : Option[(Int,Int)]= {
    val positionNearGateway = positionsLinks(position, gatewayLink)
     if(!positionNearGateway.isEmpty){
       log(s"Find a gateway in this position ${positionNearGateway.head}")
      Some(positionNearGateway.head)
    } else None
  }



  def nextPositionIsCritical(position: Seq[Int], availlableLinks: Seq[(Int, Int)]) : Option[(Int,Int)]= {
    val criticalLink = criticalLinks(availlableLinks)
    if(!criticalLink.isEmpty){
      val positions = positionsLinks(position, criticalLink)
      if(!positions.isEmpty) {
        log(s"Find a critical in this position ${positions.head}")
        Some(positions.head)
      } else {
        None
      }
    } else {
      None
    }
  }


  def findNearestCriticalPosition(position: Seq[Int], availlableLinks: Seq[(Int, Int)]) : Option[(Int,Int)] = {
    val criticalLink = criticalLinks(availlableLinks)
    log(s"Critical Link : $criticalLink")
    if(!criticalLink.isEmpty){
      nextPositionIsCritical(position, criticalLink) match {
        case Some(p) => Some(p)
        case None => {
          findNearestCriticalPosition(nextPosition(position, availlableLinks), availlableLinks)
        }
      }
    } else {
      None
    }
  }

  def findNearestGatewayPosition(position: Seq[Int], availlableLinks: Seq[(Int, Int)]) : Option[(Int,Int)]= {
    nextPositionIsGateway(position, gatewayLinks(availlableLinks)) match {
      case Some(p) => Some(p)
      case None => {
        findNearestGatewayPosition(nextPosition(position, availlableLinks), availlableLinks)
      }
    }
  }

  def loop(availlableLinks:Seq[(Int,Int)]): Unit = {
    val si = readInt // The index of the node on which the Skynet agent is positioned this turn
    log(s"Skynet position $si")
    val next = nextPositionIsGateway(Seq(si), gatewayLinks(availlableLinks))
                  .getOrElse(findNearestCriticalPosition(Seq(si), availlableLinks)
                  .getOrElse(findNearestGatewayPosition(Seq(si), availlableLinks).get))
    println(s"${next._1} ${next._2}")
    loop(availlableLinks.filterNot(t => equalsLinks(t, next)))
  }

  def nextPosition(position: Seq[Int], availlableLinks: Seq[(Int, Int)]) : Seq[Int] = positionsLinks(position, availlableLinks).map{ _._2}

  def criticalLinks(availlableLinks:Seq[(Int,Int)]) = gatewayLinks(availlableLinks).groupBy(_._1).toSeq.filter(_._2.size > 1).sortBy(_._2.size).reverse.map{c => c._2}.flatten

  def gatewayLinks(availlableLinks:Seq[(Int,Int)]) = positionsLinks(gateways,availlableLinks).map(l => if(gateways.contains(l._1)) (l._2, l._1) else l)

  def positionsLinks(positions:Seq[Int], availlableLinks:Seq[(Int,Int)]) = availlableLinks.filter(l => containsPositions(positions, l)).map(l => if(positions.contains(l._1)) l else (l._2, l._1))

  def containsPositions(positions:Seq[Int], link:(Int,Int)) = (positions.contains(link._1) || positions.contains(link._2))

  def equalsLinks(link1:(Int,Int), link2:(Int,Int)) = (link1 == link2 || link1 == (link2._2,link2._1))

  def log(s:String) = Console.err.println(s"$s")

}