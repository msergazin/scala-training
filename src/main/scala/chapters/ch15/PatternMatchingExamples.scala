package chapters.ch15

abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr


object PatternMatchingExamples {
  def simplifyTop(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => e //Double Negation
    case BinOp("+", e, Number(0)) => e // Adding zero
    case BinOp("*", e, Number(1)) => e // Multiplying by one
    case _ => expr
  }

  def identify(expr: Expr): Unit =  expr match {
    case BinOp(op, left, right) =>
      println(expr + " is a binary operation")
    case _ =>
  }

  def identify2(expr: Expr): Unit =  expr match {
    case BinOp(_,_,_) =>
      println(expr + " is a binary operation")
    case _ =>
  }

  def describe(x: Any) = x match {
    case 5 => "five"
    case true => "truth"
    case "hello" => "hi!"
    case Nil => "the empty list"
    case _ => "something else"
  }

  def generalSize(x: Any) = x match {
    case s: String => s.length
    case m: Map[_, _] => m.size
    case _ => -1
  }
}
