package manning.ch6_purely_func_state

import manning.ch6_purely_func_state.RNG.{Rand, SimpleRNG}

trait RNG {
  def nextInt: (Int, RNG)
  def rollDie: Rand[Int]
}
object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
  }
  type Rand[+A] = RNG => (A, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
      val (i,r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }
  def _double(rng: RNG): Rand[Double] = {
  //    map(nonNegativeInt)(i => i / (Int.MaxValue + 1))
      map(nonNegativeInt(_))(_ / (Int.MaxValue.toDouble + 1))
    }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i,d),r2)
    }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i,d),r) = intDouble(rng)
      ((d,i),r)
    }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1,r2) = double(rng)
      val (d2,r3) = double(r2)
      val (d3,r4) = double(r3)
      ((d1,d2,d3),r4)
    }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count <= 0)
        (List(),rng)
      else {
        val (x,r2) = rng.nextInt
        val (xs,r3) = ints(count - 1)(r2)
        (x :: xs, r3)
      }
    }
  def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
      def go(count: Int, xs: List[Int], r: RNG): (List[Int], RNG) = {
        if (count <= 0)
          (xs, rng)
        else {
          val (x,r2) = rng.nextInt
          go(count - 1, x :: xs, r2)
        }
      }
      go(count, List(), rng)
    }

    val int: Rand[Int] = _.nextInt
  //  def unit[A](a: A): Rand[A] = rng => (a,rng)
    def unit[A](a: A): Rand[A] = rng => (a,rng)
    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a,b), rng3)
      }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)((_,_))

    def nonNegativeEven: Rand[Int] =
      map(nonNegativeInt(_))(i => i - i % 2)

    val randIntDouble: Rand[(Int, Double)] = both(int, double)

  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order. It would be arguably better
  // to use `foldLeft` followed by `reverse`. What do you think?
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.
    def _ints(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

//    def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegativeInt)(_ % 2)

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, r1) = f(rng)
        g(a)(r1) // We pass the new state along
      }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }


}
  //case class State[S, +A](run: S => (A,S))





object StateExanple extends App {
  val rng = SimpleRNG(42)

  println(rng.nextInt)

//  println(rng.nonNegativeEven)
//  println(rng.int.apply(rng))
//  println(rng.int apply rng)
//  println(rng.randIntDouble.apply(rng))
//  println(rng.nonNegativeLessThan(4).apply(rng))
//  println(rng.nonNegativeLessThan(4).apply(rng))

  println(rng.rollDie(rng)._1)
//  println("list: " + rng.ints(1)(rng))
//  println("list1: " + rng.ints(5)(rng))
//  println("list2: " + rng.intsTailRecursive(5)(rng))

  /*val (n1, rng2) = rng.nextInt
  println(n1)
  println(rng2)

  val (n2, rng3) = rng2.nextInt
  println(n2)
  println(rng3)

  val (n3, rng4) = rng3.nextInt
  println(n3)
  println(rng4)
*/
  /*def randomPair(rng: RNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }*/

  /*
  println(randomPair(rng))
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }*/

//  println(rng.nonNegativeInt(rng))
//  val (n1, rng2) = rng.nonNegativeInt(rng)
//  println(n1)
//  println(rng2)
//
//  val (n2, rng3) = rng2.nonNegativeInt(rng2)
//  println(n2)
//  println(rng3)
//
//  val (n3, rng4) = rng3.double(rng3)
//  println("double " + n3)
}
