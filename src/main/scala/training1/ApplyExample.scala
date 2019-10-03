package training1

object Greet {
  def apply(name: String): String = {
    "Hello %s".format(name)
  }
}
object ApplyExample extends App {
  println(Greet.apply("Peter"))
  println(Greet("Parker"))

  val list = List.apply(1,2,3)
  val list2 = List(1,2,4)

  val func = (x: String) => "hello %s".format(x)

  // call the function

  println(func("world"))
  println(func.apply("wowr"))
}
