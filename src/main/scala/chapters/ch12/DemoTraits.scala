package chapters.ch12

//non-private vals
class Point(val x: Int, val y: Int) {
  override def toString: String = "( x , y ) : ( " + x + " , " + y + " )"
}

//x and y are still val, non-private
class Point2(x: Int, y: Int) {
  var z: Int = 0
  override def toString: String = "( x , y ) : ( " + x + " , " + y + " )"
  def getX = x
  def getY = y
//  def setX(arg: Int) = {x = arg} //error 'reassigning val x'
  def setZ(arg: Int)  = {z = arg}
}

//private x
class Point3(private var x: Int) {
}




object DemoTraits extends App {
  val point = new Point(1,2)
  println(point.x)
  println(point.toString)

  val point2 = new Point2(1,2)
  println(point2.getX)
  val point3 = new Point3(2)

//error point3.x is private
//  point3.x = 3
//  println(point3.x)


}
