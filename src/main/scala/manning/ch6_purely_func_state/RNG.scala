package manning.ch6_purely_func_state

trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG)
  def double(rng: RNG): (Double, RNG)
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double,Double,Double), RNG)
  def ints(count: Int)(rng: RNG): (List[Int], RNG)
  def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG)
}
