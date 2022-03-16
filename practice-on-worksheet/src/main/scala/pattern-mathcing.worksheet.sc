import scala.sys.Prop
// define case class
trait Expr
case class Var(name: String) extends Expr
case class Num(number: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(opertor: String, left: Expr, right: Expr) extends Expr


// pattern matching
def simplifyTop(expr: Expr): Expr =
    expr match
        case UnOp("-", UnOp("-", e)) => e
        case BinOp("+", e, Num(0)) => e
        case BinOp("*", e, Num(1)) => e
        case _ => expr
end simplifyTop

def describe(x: Any) = 
    x match
        case 5 => "fve"
        case true => "truth"
        case "hello" => "hi!"
        case Nil => "the empty list"
        case _ => "something else"
end describe

describe(5)
describe(true)
describe("hello")
describe(1)

import math.{E, Pi}
E match
    case Pi => s"strange math? pi =$Pi"
    case _ => "OK"

Pi match
    case Pi => s"strange math? pi =$Pi"
    case _ => "OK"

val pi = math.Pi
E match
    case pi => s"strange math? Pi = $pi"
    case _ => "OK"
