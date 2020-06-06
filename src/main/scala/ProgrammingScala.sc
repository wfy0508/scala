abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

def describe (x: Any) = x match{
  case 5 => "five"
  case true => "truth"
  case "hello" => "Hi!"
  case Nil => "the empty list"
  case _ => "something else"
}

describe(5)
describe(12)

import scala.math.{Pi, E}

E match{
  case Pi => "strange math! Pi = " + Pi
  case _ => "OK!"
}

val pi = 3.1415926

E match{
  case pi => "strange math! Pi = " + pi
  case _ => "OK"
}


def generalSize(x: Any) = x match{
  case s: String => s.length
  case m: Map[_, _] => m.size
  case _ => -1
}

generalSize("hello world")
generalSize(Map(1 -> 'a', 2 -> 'b'))
generalSize(math.Pi)

def isIntIntMap(x: Any) = x match{
  case m: Map[Int, Int] => m.size
  case _ => false
}

isIntIntMap(Map(1 -> 1))
isIntIntMap(Map("abc" -> "cde"))

def simplifyAll(expr: Expr): Expr = expr match{
  case UnOp("-", UnOp("-", e)) => simplifyAll(e)
  case BinOp("+", e, Number(0)) => simplifyAll(e)
  case BinOp("*", e, Number(1)) => simplifyAll(e)
  case UnOp(op, e) => UnOp(op, simplifyAll(e))
  case BinOp(op, l, r) => BinOp(op, simplifyAll(l), simplifyAll(r))
  case _ =>expr
}

val e = Var("x")

simplifyAll(e)

val x = Number(-123)

simplifyAll(x)

val y = UnOp("-", x)
simplifyAll(y)