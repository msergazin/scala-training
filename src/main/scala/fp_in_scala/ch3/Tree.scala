package fp_in_scala.ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }
  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }
  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))
  }
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def main(args: Array[String]) = {
    val l1 = Leaf(1)
    val l2 = Leaf(2)
    val l3 = Leaf(3)
    val l4 = Leaf(4)
    val branch12 = Branch(l1,l2)
    val branch34 = Branch(l3,l4)
    val tree = Branch(branch12, branch34)

    println(size(tree))
  }
}
