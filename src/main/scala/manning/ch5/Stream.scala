package manning.ch5

import manning.ch5



sealed trait Stream[+A] {
  /*constructor for creating a nonempty stream.*/
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)
  }
  /*constructor for creating an empty stream of a particular type.*/
  def empty[A]: Stream[A] = Empty
  def apply[A](s: A*): Stream[A] = {
    if (s.isEmpty) empty else cons(s.head, apply(s.tail: _*)) //need type for s.tail: _*
  }
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => Nil
  }
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /*take(n) for returning the first n elements of a Stream*/
  def take(n: Int): Stream[Any] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /*drop(n) for skipping the first n elements*/
  def drop(n: Int): Stream[A] = {
    def go(s: Stream[A], count: Int): Stream[A] = s match {
      case Cons(h,t) => if (count > 0) go(t(), count) else cons(h(), go(t(), count - 1))
      case _ => Empty
    }
    go(this, n)
  }
}
case object Empty extends Stream[Nothing]
/*The arguments we’d like to pass unevaluated have a () => immediately before their
type. A value of type () => A is a function that accepts zero arguments and returns an
A. In general, the unevaluated form of an expression is called a thunk, and we can force
the thunk to evaluate the expression and get a result.*/
/*nonempty stream consists of a head and a tail, which are both non-strict.
Due to technical limitations, these are thunks that must be explicitly forced,
rather than by-name parameters*/
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream extends App with Stream[String] {
  def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0
//  maybeTwice(true, {println("hi"); 1+41})

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }
//  maybeTwice2(true, {println("hi"); 1+41})

  val stream2: Stream[Any] = cons({ () => 42}, Stream.empty)
  val stream1: Stream[Any] = cons(1, cons(2, Stream.empty) )
  val stream3: Stream[Int] = cons(1, Stream.empty)
//  println(s"Elements of stream1 = ${stream1.headOption}")
//  println(stream1.toList)
  println(stream1.take(2).headOption)
  println(stream1.drop(1).headOption)
  println(stream3.take(2).headOption)

}
