package training1

import scala.annotation.tailrec


object ImperativeVsFunctionalExamples extends App  {

  println(fibImperative(7))
  println(fibTailRecuresive(7))
  println(fibRecursive(7))

  def factorial(n: Int): Int = {
    if (n == 0) 1 else n * factorial(n-1)
  }


  def fibRecursive(n:Int) : Int =
    if ( n <= 0 ) 0
    else if (n == 1) 1
    else fibRecursive(n-1) + fibRecursive(n-2)

  /**
   * – Well suited for small numbers
   * – If n is big, we run the risk of getting a Stack Overflow
   *
   * A recursive function is tail recursive when recursive call is the last thing executed by the function
   * */

  def fibTailRecuresive(x: Int): BigInt = {
    @tailrec def fibHelper(x: Int, prev: BigInt = 0, next: BigInt = 1): BigInt = x match {
//    def fibHelper(x: Int, prev: BigInt = 0, next: BigInt = 1): BigInt = x match {
      case 0 => prev
      case _ => fibHelper(x - 1, next, (next + prev))
    }
    fibHelper(x)
  }



  /**
   * – Handles Integer numbers (32 bit)
   * – Too verbose, non-idiomatic, mutable variables.
   */
  def fibImperative(nthNumber : Int ) : Int = {
    var first = 0
    var second = 1
    var count = 0
    var sum = 0

    while(count < nthNumber){
      sum = first + second
      first = second
      second = sum
      count = count + 1
    }
    first
  }
}
