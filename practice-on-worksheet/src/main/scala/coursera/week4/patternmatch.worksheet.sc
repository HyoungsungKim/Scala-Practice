trait Expr:
    def isNumber: Boolean
    def isSum: Boolean
    def numValue: Int
    def leftOp: Expr
    def rightOp: Expr

class Number(n: Int) extends Expr:
    def isNumber = true
    def isSum = false
    def numValue = n
    def leftOp = throw Error("Number.leftOp") 
    def rightOp = throw Error("Number.rightOp")
end Number

class Sum(e1: Expr, e2: Expr) extends Expr:
    def isNumber = false
    def isSum = true
    def numValue = throw Error("Sum.numValue")
    def leftOp = e1
    def rightOp = e2
end Sum

def eval(e: Expr): Int = 
    if e.isNumber then e.numValue
    else if e.isSum then eval(e.leftOp) + eval(e.rightOp)
    else throw Error("Unknown expression " + e)
end eval

val num1 = new Number(1)
val num2 = new Number(2)

num1.numValue
num2.numValue

eval(Sum(num1, num2))

// OO decomposition mix data and operation
// so we lose original data

// Functional Decomposition with Pattern mathcning

// Case Classes
// A case class definition is similar to a normal class definition,
// except that is is preceded by the modifier case
trait CaseExpr
case class CaseNumber(n: Int) extends CaseExpr
case class CaseSum(e1: CaseExpr, e2: CaseExpr) extends CaseExpr
case class CaseProd(e1: CaseExpr, e2: CaseExpr) extends CaseExpr
// these classes are not empty

def pmEval(e: CaseExpr): Int = e match
    case CaseNumber(n) => n
    case CaseSum(e1, e2) => pmEval(e1) + pmEval(e2)

val cs = new CaseSum(new CaseNumber(1), new CaseNumber(2))
pmEval(cs)

def show(e: CaseExpr): String = e match
    case CaseNumber(n) => n.toString()
    case CaseSum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case CaseProd(e1, e2) => s"${showP(e1)} * ${showP(e2)}"
        // e1 match
            //case CaseNumber(n1) => s"${show(e1)} * ${show(e2)}"
            //case CaseSum(n1, n2) => s"(${show(e1)}) * ${show(e2)}"
end show 

def showP(e: CaseExpr): String = e match
    case e: CaseSum => s"(${show(e)})"
    case _ => show(e)

println(show(new CaseNumber(1)))
println(show(cs))

val sum = new CaseSum(new CaseProd(new CaseNumber(1), new CaseNumber(2)), new CaseNumber(3))
val prod = new CaseProd(new CaseSum(new CaseNumber(1), new CaseNumber(2)), new CaseNumber(3))
show(sum)
show(prod)