package chapters.ch12

class Point(val x: Int, val y: Int)
class Rectangle(val topLeft: Point, val bottomRight: Point) extends Rectangular {
  // and many more geometric methods...
}

trait Rectangular {
  def topLeft: Point
  def bottomRight: Point
  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
  // and many more geometric methods...
  def saySomething() = {
    println("Everybody says: 'Say something'")
  }
}

abstract class Component extends Rectangular {
  // and many more geometric methods...
}

object RectangularObjects extends App {
  val rect = new Rectangle(new Point(1, 1),
    new Point(10, 10))
  rect.saySomething()


}
