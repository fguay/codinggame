import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Chuck extends App {
  val message = "%"

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  Console.err.println("message : " + message)


  val binaryMessage = (for {
    c <- message
    b <- {
      val f = Integer.toBinaryString(c.toByte)
      val fl = f.length
      if (fl < 7) "0" * (7 - fl) + f  else f
    }
  } yield (b)).toList

  Console.err.println("binary view : " + binaryMessage.mkString)

  def loop(acc:List[String], b:Char, r:List[Char]) : List[String] = {
    Console.err.println("ACC :" + acc)
    if (r.isEmpty) {
      Console.err.println("End return acc :" + acc)
      acc
    } else {
      val f = r.head
      Console.err.println("Current :" + f)
      if (f == b) {
        Console.err.println("Equals add 0 to acc" + acc)
        loop(acc ::: ("0" :: Nil), b, r.tail)
      } else {
        val nacc = if(!acc.isEmpty) acc ::: (" " :: Nil) else acc

        f match {
          case '0'  => {
            Console.err.println("Diff change byte to" + f + "add '00 0' to acc : " + acc)
            loop(nacc ::: ("00 0" :: Nil),f, r.tail )
          }
          case '1' => {
            Console.err.println("Diff change byte to" + f + "add '0 0' to acc : " + acc)
            loop(nacc ::: ("0 0" :: Nil),f, r.tail )
          }
        }
      }
    }
  }

  val res = loop(Nil, ' ', binaryMessage)
  Console.err.println("chuck : " + res.mkString)
  println(res.mkString)

}
