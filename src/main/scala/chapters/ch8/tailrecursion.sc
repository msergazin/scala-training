//tail recursion
/*This function is not tail recursive, because it performs an increment operation after the recursive call.
You'll get what you expect when you run it:
scala> boom(3)
java.lang.Exception: boom!
at .boom(<console>:5)
at .boom(<console>:6)
at .boom(<console>:6)
at .boom(<console>:6)
at .<init>(<console>:6)
*/
def boom(x: Int): Int =
if (x == 0) throw new Exception("boom!")
else boom(x - 1) + 1

boom(3)



/*This time, you see only a single stack frame for bang. You might think that bang crashed before it
called itself, but this is not the case.*/
def bang(x: Int): Int =
  if (x == 0) throw new Exception("banh!")
  else bang(x - 1)

bang(3)
