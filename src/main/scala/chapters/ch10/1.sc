abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
}

//val ele = new Element

val list = List(1,2,3)
list.map(_ + 1)
