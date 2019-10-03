package chapters.ch12
/*
a parameter whose sole purpose is to be copied into the some field:
combining the parameter and the field in a single parametric
field definition, as shown*/

//non-private vals
/*val - is a shorthand that defines at the same
time a parameter and unreassignable field with the same name, which CAN be accessed from outside the class*/
class Point1(val x: Int, val y: Int) {
  override def toString: String = "( x , y ) : ( " + x + " , " + y + " )"
}
/*var - is a shorthand that defines at the same
time a parameter and reassignable field with the same name, which CAN be accessed from outside the class*/
class Point4(var x: Int)

//if no val or var modifier for a parametric field (like x), then it's val and private. (?)


//x and y are still val, fields appear to be private
class Point2(x: Int, y: Int) {
  var z: Int = 0
  override def toString: String = "( x , y ) : ( " + x + " , " + y + " )"
  def getX = x
  def getY = y
  //  def setX(arg: Int) = {x = arg} //error 'reassigning val x'
  def setZ(arg: Int)  = {z = arg}
}
//private x
class Point3(private var x: Int)

object ParametricFieldDefinitions extends App {
  val point = new Point1(1,2)
  println(point.x)
  println(point.toString)

  val point2 = new Point2(1,2)
//  println(point2.x)
  println(point2.getX)
  val point3 = new Point3(2)

//error point3.x is private
//  point3.x = 3
//  println(point3.x)

  val point4 = new Point4(1)
  point4.x = 5
  println(point4.x)


}
