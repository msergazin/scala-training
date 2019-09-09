package training1.objectsNclasses
//primary cons
class Car(speed: Double, color: String) {
  def getSpeed(): Double = { speed }
  def getColor = color
}

object Main extends App {
  /*extending app gives def main(args: Array[String]){}*/
  val car1 = new Car(1, "red")
  println(car1.getSpeed())
}