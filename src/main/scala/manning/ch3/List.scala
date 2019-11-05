package manning.ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
/*it's like a node that has head and next pointers*/
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
//    foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t) //works
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h,t) => Cons(f(h), t))
  }
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))
  def add1(list: List[Int]): List[Int] = {
    foldRight(list, Nil:List[Int])((h,t) => Cons(h+1, t))
  }
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_,_))
  def length[A](as: List[A]): Int = {
//    foldRight(as, 0)((_,acc) => acc + 1)
    foldLeft(as, 0)((acc, h) => acc + 1)
  }

  /*1->2->3->Nil
  * 1->(2->(3->List())
  * 1->(2->(3)
  * 1->(2<-(3)
  * 1<-2<-3
  *
  * */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

//  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        /*in case of sum, we start adding head to the initial sum of 0*/
        case Cons(h,t) => foldLeft(t, f(z,h))(f)
      }

  }

//  def sum(ints: List[Int]): Int = foldRight(ints, 0)((x,y) => x + y)
  /*
  *
  * foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
    1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => x + y)
    1 + (2 + foldRight(Cons(3, Nil), 0)((x,y) => x + y))
    1 + (2 + (3 + (foldRight(Nil, 0)((x,y) => x + y))))
    1 + (2 + (3 + (0)))
    6
   */
//  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
//  def product(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
  def product(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  /*def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }*/
  /*def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }*/
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }
  def setHEad[A](head: A, list: List[A]): List[A] = {
    list match {
      case Nil => Cons(head, Nil)
      case Cons(_, t) => Cons(head, t)
    }
  }
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop( t, n - 1)
    }
  }
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }
  /*a List consisting of all but the last element of a List*/
  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_,Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }


  def main(args: Array[String]) = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))
    val list: List[Double] = Cons(1,Cons(2, Cons(3, Nil)))

    //    println(product(list))
//    println(drop(list, 2))
//    println(dropWhile(list, ((x: Double) => x  != 2)))
//    println(dropWhile(list)(_  != 2))
//    println(init(list))
//    println(doubleToString(list))
//    println(map(list)(_ + 1))
    println(filter(list)(_ % 2 == 0))

  }
}
