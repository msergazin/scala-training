package fp_in_scala.ch2

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  def formatResult(message: String, n: Int, f: Int => Int) = {
    message + " of " + n + " = " + f(n)
  }
  def tailRecursiveFactorial(n: Int): Int = {
    def go(n: Int, result: Int): Int = {
      if (n == 1) result
      else go(n - 1, n * result)
    }
    go(n,1)
  }
  def recursiveFactorial(n: Int): Int = {
    if (n == 1) 1
    else n * recursiveFactorial(n - 1)
  }
  def fibonacci(n: Int): Int = {
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else go(n - 1, curr, curr + prev)
    }
    go(n, 0, 1)
  }
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n),as(n + 1))) false
      else go(n + 1)
    }
    go(0)
  }
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
  def main(args: Array[String]) = {
    println(
      isSorted(
        Array(1,0,3),
        (x: Int, y: Int) => if (x <= y) true else false
      )
    )

//    println(tailRecursiveFactorial(15))
//    println(recursiveFactorial(15))
//    println(fibonacci(8))
//    println(formatResult("factorial", 5, tailRecursiveFactorial))
  }
}
