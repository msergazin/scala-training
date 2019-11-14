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
  def headOption: Option[A] = foldRight(None: Option[A])((a,_) => Some(a))
  /*this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }*/

  /*take(n) for returning the first n elements of a Stream*/
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /*drop(n) for skipping the first n elements*/
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }
  /*returning all starting elements of a Stream that match the given predicate*/
  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h,t)
      else      empty)

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) =>
      if (p(h())) cons(h(),t().takeWhile(p))
      else t().takeWhile(p)
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
  //    this match {
  //      case Cons(h, t) => p(h()) || t().exists(p)
  //      case _ => false
  //     }//works
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  /*checks that all elements in the Stream match a given predicate.*/
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)
  /*this match {
    case Cons(h,t) =>
      if (p(h())) t() forAll p
      else false
    case _ => true
  }*/
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (f(a)) cons(a,b) else b)
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
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
  val stream5: Stream[String] = cons("1", cons("2", Stream.empty) )
  val stream3: Stream[Int] = cons(1, Stream.empty)
  val stream4: Stream[Int] = cons(1,cons(2,cons(3,cons(4,cons(5,cons(6,cons(7, Stream.empty)))))))
//  println(s"Elements of stream1 = ${stream1.headOption}")
//  println(stream1.toList)
  assert(stream1.take(2).headOption ==Some(1))
  assert(stream1.drop(1).headOption == Some(2))
  assert(stream3.take(1).headOption == Some(1))
//  assert(stream4.takeWhile(_ % 2 == 0).toList == List(2,4,6))
  println(stream4.takeWhile(_ % 2 == 0).toList)
  assert(stream4.forAll(_ < 8) == true)
  assert(stream4.forAll(_ != 7) == false)
  assert(stream4.map(_ * 4).toList == List(4, 8, 12, 16, 20, 24, 28))
  assert(stream4.filter(_ % 2 == 0).toList == List(2, 4, 6))
  assert(stream4.append(stream4).toList == List(1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7))

  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones().toList)

}
