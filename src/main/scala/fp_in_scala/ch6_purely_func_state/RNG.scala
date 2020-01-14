package fp_in_scala.ch6_purely_func_state

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

trait RNG {
  def nextInt: (Int, RNG)
}
object RNG  extends App {
  type Rand[+A] = RNG => (A, RNG)
  val int:Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }
  //  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
//  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i,rng2) => (i % 2 == 0,rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  def double_1: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i ,r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i,d), r3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r2) = intDouble(rng)
    ((d,i),r2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1,d2,d3),r4)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, list: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n <= 0) (list, rng)
      else {
        val (i,r1) = rng.nextInt
        go(n - 1, i :: list, r1)
      }
    }
    go(count, List(), rng)
  }
  def nonNegativeEven: Rand[Int] = {
    /*rng =>
      map(nonNegativeInt)(i => i - i % 2)(rng)*/ //same as
    map(nonNegativeInt)(i => i - i % 2)
  }
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
    /*rng =>{
      val (v1, r1) = ra(rng)
      val (v2, r2) = rb(r1)
      (f(v1,v2), r2)
    }*/
  }
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    map2(ra, rb)((_, _))
  }
  val randIntDouble: Rand[(Int, Double)] = {
    both(int, double)
  }
  val randDoubleInt: Rand[(Double, Int)] = {
      both(double, int)
    }
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }

  val list = List(1,2,3)
  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  assert(n1 == 16159453)
  val (n2, rng3) = rng2.nextInt
  assert(n2 == -1281479697)
  val (n3, rng4) = nonNegativeInt(rng3)
  assert(n3 == 340305901)
  assert(ints(2)(rng)._1 == List(-1281479697, 16159453))
  assert(int(rng)._1 == 16159453)
  assert(unit(1)(rng) == (1,SimpleRNG(42))) //rng is not used
  assert(nonNegativeEven(rng)._1 == 16159452)
  assert(double_1(rng)._1 == 0.007524831686168909)
  assert(randIntDouble(rng)._1 == (16159453,0.5967354848980904))
}


import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
