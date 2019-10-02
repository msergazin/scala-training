package chapters.ch12

class Frog extends Philosophical {
  override def toString: String = "green"
}

object Frog extends App {
  val frog = new Frog()
  frog.philosophize()
  println(frog.toString)
}