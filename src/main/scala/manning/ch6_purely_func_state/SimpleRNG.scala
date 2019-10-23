package manning.ch6_purely_func_state

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  override def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
  override def _double(rng: RNG): Rand[Double] = {
//    map(nonNegativeInt)(i => i / (Int.MaxValue + 1))
    map(nonNegativeInt)(_ / (Int.MaxValue + 1))
  }

  override def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i,d),r2)
  }
  override def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }
  override def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1,r2) = double(rng)
    val (d2,r3) = double(r2)
    val (d3,r4) = double(r3)
    ((d1,d2,d3),r4)
  }
  override def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (List(),rng)
    else {
      val (x,r2) = rng.nextInt
      val (xs,r3) = ints(count - 1)(r2)
      (x :: xs, r3)
    }
  }
  override def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
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
  def unit[A](a: A): Rand[A] = rng => (a,rng)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)
}

object SimpleRNG extends App {
  val rng = SimpleRNG(42)
//  println(rng.nonNegativeEven)

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
  val (n1, rng2) = rng.nonNegativeInt(rng)
  println(n1)
  println(rng2)

  val (n2, rng3) = rng2.nonNegativeInt(rng2)
  println(n2)
  println(rng3)

  val (n3, rng4) = rng3.double(rng3)
  println("double " + n3)
}
