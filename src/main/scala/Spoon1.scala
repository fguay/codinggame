import math._
import scala.util._

/**
  * Don't let the machines win. You are humanity's last hope...
  **/
object Spoon1 extends App {
  val width = readInt // the number of cells on the X axis
  val height = readInt // the number of cells on the Y axis

  def none(x:Int, y:Int) : String = "-1 -1"
  def right(x:Int, y:Int) : String = nextPoint(x+1,y)(right)
  def bottom(x:Int, y:Int) : String = nextPoint(x,y+1)(bottom)

  val game = (0 until height).map(_ => readLine.toList).toList
  Console.err.println("width : " + width + " height" + height + " game " + game)

  val move = for{
    x <- (0 until width)
    y <- (0 until height)
  } yield{
    (nextPoint(x, y)(none):: right(x, y) :: bottom(x, y) :: Nil).mkString(" ")
  }

  def nextPoint(x:Int, y:Int)(move: (Int,Int) => String) : String = {
    if(x >= width) "-1 -1"
    else if(y >= height) "-1 -1"
    else game(y)(x) match {
      case '0' => x + " " + y
      case _ => move(x, y)
    }
  }

  move.filterNot(_.startsWith("-1")).map(println(_))

}