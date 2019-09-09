package training1.objectsNclasses

class Vehicle(model: String, year: Int) {
  def this(model: String, year: Int, mileage: Int) = {
    // Invoking primary constructor
    this(model, year)
    this.mileage = mileage
  }
  var mileage: Int = 0
  def details(): String = {
    year + " " + model + " with " + mileage + "L/100 KM"
  }

}
object Main4 extends App {
  val car = new Vehicle("Camry", 2018, 10)
  println(car.details())
}