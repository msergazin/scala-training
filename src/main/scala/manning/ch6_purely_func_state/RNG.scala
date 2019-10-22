package manning.ch6_purely_func_state

trait RNG {
  def nextInt: (Int, RNG)
}
