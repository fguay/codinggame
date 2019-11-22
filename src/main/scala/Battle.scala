import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Battle extends App {

  val (p1,p2) = shuffle()
  val emptyStack = (Seq[Int](),Seq[Int]())

  println(battle(p1, p2, emptyStack , 0))

  def battle(p1:Seq[Int], p2:Seq[Int], stack:(Seq[Int],Seq[Int]), round:Int): String = {
    (p1, p2) match {
      case (Seq(), Seq()) => "PAT"
      case (p1, Seq()) => s"1 $round"
      case (Seq(), p2) => s"2 $round"
      case (p1, p2) if (p1.head > p2.head) => battle(p1.tail ++ (stack._1 :+ p1.head) ++ (stack._2 :+ p2.head), p2.tail, emptyStack, round+1)
      case (p1, p2) if (p1.head < p2.head) => battle(p1.tail, p2.tail  ++ (stack._1 :+ p1.head) ++ (stack._2 :+ p2.head), emptyStack,round+1)
      case (p1,p2) if(p1.tail.size < 4 || p2.tail.size < 4) => "PAT"
      case (p1,p2) => {
        val dp1 = p1.splitAt(4)
        val dp2 = p2.splitAt(4)
        battle(dp1._2, dp2._2, (stack._1 ++ dp1._1, stack._2 ++ dp2._1), round)
      }
    }
  }

  def shuffle(): (Seq[Int], Seq[Int] ) = {
    val n = readInt // the number of cards for player 1
    val p1 = (0 until n).map(_ => readLine).map(convertCardToInt(_))
    val m = readInt // the number of cards for player 2
    val p2 = (0 until m).map(_ => readLine).map(convertCardToInt(_))
    (p1,p2)
  }

  def convertCardToInt(str: String) = str.dropRight(1) match {
    case "J" => 11
    case "Q" => 12
    case "K" => 13
    case "A" => 14
    case other => other.toInt
  }

}

