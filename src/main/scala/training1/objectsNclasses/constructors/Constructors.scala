package training1.objectsNclasses.constructors

//primary cons
class Car(speed: Double, color: String) {
  def getSpeed(): Double = { speed }
  def getColor = color

  //default primary constructor. It does not contain any parameter.
  def this() = {
    this(0,"black")
  }
}

//primary cons
class CarVarParams(var speed: Double, color: String) {
  def getColor = color
}

object Constructors extends App {
  val car1 = new Car(1, "red")
  val carDef = new Car()
  println(car1.getSpeed())
  println(carDef.getSpeed())

  val carVar = new CarVarParams(1, "red")
  println(carVar.speed)

  carVar.speed = 24
  println(carVar.speed)
}
