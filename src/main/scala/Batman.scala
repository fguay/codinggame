import math._
import scala.annotation.tailrec
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Batman extends App {
  // w: width of the building.
  // h: height of the building.
  val Array(w, h) = for(i <- readLine split " ") yield i.toInt
  val n = readInt // maximum number of turns before game over.
  val Array(x0, y0) = for(i <- readLine split " ") yield i.toInt

  case class Tree(weight: Node, height: Node)

  case class Node(min:Int, max:Int, pos:Int)

  def nextNode(node:Either[Node,Node]): Node = {
    node match {
      case Left(l) => Node(l.min,l.pos, l.pos - math.ceil((l.pos - l.min).toDouble / 2).toInt )
      case Right(r) => Node(r.pos, r.max, r.pos + math.ceil((r.max - r.pos).toDouble / 2).toInt )
    }
  }

  @tailrec
  def loop (tree:Tree) : Tree= {
    val move = readLine
    Console.err.println("Go to : " + move)
    val subTree:Tree = move.toSeq.foldLeft(tree){ (bat, c) =>
      c match {
        case 'L' => Tree(nextNode(Left(bat.weight)), bat.height)
        case 'R' => Tree(nextNode(Right(bat.weight)), bat.height)
        case 'U' => Tree(bat.weight, nextNode(Left(bat.height)))
        case 'D' => Tree(bat.weight, nextNode(Right(bat.height)))
      }
    }
    println(subTree.weight.pos + " " + subTree.height.pos)
    loop(subTree)
  }

  loop(Tree(Node(0, w, x0),Node(0, h, y0)))

}