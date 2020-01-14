package training5

import scala.util.{Try,Success,Failure}

object HandlingErrorsWithoutExceptions extends App {
  val a = toInt("1")
  val b = toInt("boo")
  def toInt(s: String): Try[Int] = Try {
    Integer.parseInt(s.trim)
  }

  toInt("boo") match {
    case Success(i) => println(i)
    case Failure(s) => println(s"Failed. Reason: $s")
  }

}