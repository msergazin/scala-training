package fp_in_scala.ch5

import fp_in_scala.ch5



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
  def take_1(n: Int): Stream[A] =
    unfold((this,n)){
      case (Cons(h, _), 1) => Some((h(), (empty,0)))
      case (Cons(h, t), _) => Some((h(), (t(),n - 1)))
      case _ => None
    }
  /*drop(n) for skipping the first n elements*/
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }
  /*returning all starting elements of a Stream that match the given predicate*/
  def takeWhile_2(f: A => Boolean): Stream[A] =
    /*
    //my solution - slightly more complicated
    unfold((this, f(this.headOption.get))){
      case (Cons(h,t), true) =>Some((h(),(t(), true)))
      case _ => None
    }*/
    unfold(this) {
      case Cons(h,t) if f(h()) => Some((h(), t()))
      case _ => None
    }
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
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
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
  def map_1[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (f(a)) cons(a,b) else b)
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant_1[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def constant_2[A](a: A): Stream[A] =
    unfold(a)(n => Some(a,a))
  def ones_1: Stream[Int] =
    constant_1(1)
  def ones_2: Stream[Int] =
    unfold(1)(_ => Some(1,1))
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))
  def from_1(n: Int): Stream[Int] =
    unfold(n)(n => Some((n,n+1)))
  def fibs(n: Int): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }
  /*It takes an initial state, and a function for producing both the next state and the next value in the generated stream*/
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal,
  until the point that `s2` is exhausted. If `s` is exhausted first, or we find an
  element that doesn't match, we terminate early. Using non-strictness,
  we can compose these three separate logical steps--the zipping,
  the termination when the second stream is exhausted, and the termination if
  a non-matching element is found or the first stream is exhausted.
  */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
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
  val stream5: Stream[String] = cons("1", cons("2", Stream.empty) )
  val stream3: Stream[Int] = cons(1, Stream.empty)
  val stream4: Stream[Int] = cons(1,cons(2,cons(3,cons(4,cons(5,cons(6,cons(7, Stream.empty)))))))
//  println(s"Elements of stream1 = ${stream1.headOption}")
//  println(stream1.toList)
  assert(stream1.take(2).headOption ==Some(1))
  assert(stream1.drop(1).headOption == Some(2))
  assert(stream3.take(1).headOption == Some(1))
//  assert(stream4.takeWhile(_ % 2 == 0).toList == List(2,4,6))
//  println(stream4.takeWhile(_ % 2 == 0).toList)
  assert(stream4.forAll(_ < 8) == true)
  assert(stream4.forAll(_ != 7) == false)
  assert(stream4.map(_ * 4).toList == List(4, 8, 12, 16, 20, 24, 28))
  assert(stream4.filter(_ % 2 == 0).toList == List(2, 4, 6))
  assert(stream4.append(stream4).toList == List(1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7))

  val ones: Stream[Int] = Stream.cons(1, ones)
  assert(ones.exists(_ % 2 != 0) == true)
  assert(ones.map(_ + 1).exists(_ % 2 == 0) == true)
  //  println(ones.forAll(_ == 1)) //exception
  assert(constant(1).exists(_ % 2 != 0) == true)
  assert(stream4.map_1(_ + 1).toList == List(2, 3, 4, 5, 6, 7, 8))
  println(stream4.takeWhile_1(_ != 3).toList)
}
