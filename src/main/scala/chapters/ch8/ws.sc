val list = List(-3,-1,0,4,5)
list.filter(_ > 0)
val f = (_: Int) + 2 * (_: Int) * (_: Int)
f(3,4,5)
def sum(a: Int, b: Int, c: Int) = a+b+c
sum(3,4,5)
val a = sum _
a(1,2,3)

def speed(distance: Float, time: Float): Float = distance / time
speed(time = 3, distance = 10)