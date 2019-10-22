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
    if (i < 0) (-(i + 1),r) else (i,r)
  }


  /*type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)*/
}

object SimpleRNG extends App {
  val rng = SimpleRNG(42)
  /*val (n1, rng2) = rng.nextInt
  println(n1)
  println(rng2)

  val (n2, rng3) = rng2.nextInt
  println(n2)
  println(rng3)

  /*def randomPair(rng: RNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }*/

  println(randomPair(rng))
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }*/

  println(rng.nonNegativeInt(rng))

}
