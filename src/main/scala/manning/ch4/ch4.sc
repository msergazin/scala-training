def failingFn(i: Int): Int = {
  println("a")
  val y: Int = throw new Exception("fail!")
  println("asd")
  try {
    val x = 42 + 5
    x + y
  }
  catch { case e: Exception => 43 }
}

def failingFn2(i: Int): Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail!")): Int)
  }
  catch { case e: Exception => 43 }
}

//failingFn(2)
failingFn2(3)