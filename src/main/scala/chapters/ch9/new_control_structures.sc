def twiceApplyAnOperation(op: Double => Double, x: Int)  =
{
  op(op(x))
}
twiceApplyAnOperation((x => x + 2), 5)
twiceApplyAnOperation(_ + 1, 5)
