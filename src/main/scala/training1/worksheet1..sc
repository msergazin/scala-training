def acceptFunc(n: Int, f:(Double, Double) => Double): Double = {
  f(n, n + 2)
}

def func1(a: Double, b: Double): Double = {
  a * a / b
}

def func2(a: Double, b: Double): Double = {
  a / (a * b)
}

acceptFunc(3, func1)
acceptFunc(3, func2)

