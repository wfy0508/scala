sealed abstract class Expr //声明为密封类
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

def descirbe(e: Expr): String = (e: @unchecked) match{
  case Number(_) => "a number"
  case Var(_) => "a variable"
}

val capital = Map("France" -> "Pairs", "Japan" -> "Tokyo")
capital get "France"
capital get "America"