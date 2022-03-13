 trait Expr
 object Expr:
     case class Var(s: String) extends Expr
     case class Number(n: Int) extends Expr

val a = Expr.Var("Hello")
a.s

import Expr.*

Var("Hello")
Number(1)
val n = Number(1)
n

enum Expr2:
    case Var2(s: String)
    case Number2(n: Int)

import Expr2.*
Expr2.Var2("Hello")
Var2("Hello")
// Var2.s Error!
Number2(1)
// val a = Number2(1)  Error!

// enum은 constant처럼 사용

enum Direction(val dx: Int, val dy: Int):
    case Right extends Direction(1, 0)
    case Up extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Down extends Direction(0, -1)

    def leftTurn = Direction.values((ordinal + 1) % 4)
end Direction

val r = Direction.Right
r.ordinal
val u = r.leftTurn
u.ordinal
Direction.values(0)
Direction.values(1)
Direction.values(2)
Direction.values(3)
val v = (u.dx, u.dy)