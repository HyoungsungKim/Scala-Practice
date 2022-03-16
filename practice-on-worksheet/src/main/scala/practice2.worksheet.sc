class Rational(n: Int, d: Int):
    // println("Created " + n + "/" + d)
    require(d != 0)

    private val g = gcd(n.abs, d.abs)
    val numer: Int = n / g 
    val denom: Int = d / g 

    def this(n: Int) = this(n, 1)

    override def toString = s"$n/$d"


    def add(that: Rational): Rational = 
        Rational(this.numer * that.denom + that.numer * d, d * that.denom)

    def lessThan(that: Rational) = 
        this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) = 
        if this.lessThan(that) then that else this

    def * (that: Rational): Rational = 
        Rational(numer * that.numer, denom * that.denom)

    private def gcd(a: Int, b: Int): Int = 
        if b == 0 then a else gcd(b, a % b)

end Rational

object Rational:
    def apply(n:Int, d:Int) = new Rational(n * 2, d * 2)
    def apply(n:Int) = new Rational(n)
end Rational

val rat:Rational = new Rational(1,2)
val rat2:Rational = Rational(1,2)
rat
rat.numer

rat.add(rat2)

val rat3 = new Rational(1,2)
val rat4 = new Rational(3)
val rat5 = Rational(3)

rat.*(rat2)

List(1,2,3).filter(x => x<3)

List(1,2,3,4,5).filter((elem:Int) => elem < 4)

1 :: (2 :: Nil)

val a :: b :: c :: d = List(1,2,3)

val q :: rest = List(1, 2, 3)
Set(1)
Set().isEmpty
Set(1,2,3).tail

Set(1) ++ Set(2)
Set(1) + 2
Set(1) + 1

def merge[T](xs:List[Int], ys:List[Int]): List[Int] = (xs, ys) match
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (xsHead :: xsTail, ysHead :: ysTail) => 
        if xsHead > ysHead then xsHead :: merge(xsTail, ys)
        else ysHead :: merge(xs, ysTail)

extension [T](xs:List[T]) 
    def splitAt(n: Int) = (xs.take(n), xs.drop(n))



def msort(xs: List[Int]):List[Int] =
    val n = xs.length/2
    if n == 0 then xs
    else
        val (left, right) = xs.splitAt(n)
        merge(msort(left), msort(right))

merge(List(2,1,3), List(5,4,6))
msort(List(2,1,3,5,4,6))

List(1) ++ List(2)

Set(1) ++ Set(2)

List(1,2,3).exists(_ == 1)

    
"asd".contains("a")
"asd".contains(List("a", "s"))
Nil
"a" < "b"
"b" < "a"

math.max(1,2)

List((1,1)) ++ List((2,2))

def times(chars: List[Char]): List[(Char, Int)] = chars match
    case Nil => Nil
    case List(ch) => List((ch, 1))
    case head :: tail => 
      val (target, remain) = tail.partition(ch => ch == head)
      List((head, target.length + 1)) ++ times(remain)

val ch:List[Char] = List('a', 's', 'd', 'a', 's', 'd')
times(ch)

Set(3) ++ Set(1) ++ Set(2)

var tp = List((2,2), (1,1), (3,3))
tp.sortBy(_._2)

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

List(1) ++ List(0)
List() ++ List(1)

List(1,2,3).find(_==1)