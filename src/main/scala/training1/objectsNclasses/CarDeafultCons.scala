package training1.objectsNclasses

//default primary constructor. It does not contain any parameter.
class CarDefaultCons {
  def display(): Unit = {
    println("displaying stuff")
  }
}

object Main2 extends App {
  /*extending app gives def main(args: Array[String]){}*/
  val car1 = new CarDefaultCons()
  car1.display()
}