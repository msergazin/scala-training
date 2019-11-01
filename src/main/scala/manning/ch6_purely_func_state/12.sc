def useFunc(a: Int)(f: Int => Int) = { f(a) }
useFunc(3)(i => i * i)


