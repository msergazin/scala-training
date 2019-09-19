package training1.objectsNclasses

//primary cons
class CarVarParams(var speed: Double, color: String) {
  def getColor = color
}

object Main3 extends App {
  /*extending app gives def main(args: Array[String]){}*/
  val car1 = new CarVarParams(1, "red")
  println(car1.speed)

  car1.speed = 24
  println(car1.speed)
}