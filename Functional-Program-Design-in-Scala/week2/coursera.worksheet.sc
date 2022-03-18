import pprint.Tree.Lazy
abstract class IntSet:
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
    def union2(other: IntSet): IntSet

object Empty extends IntSet:
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
    def union(other: IntSet) =
        other
    def union2(other: IntSet) =
        other
    /*
        = other match
        case Empty => Empty
        case NonEmpty(_, _, _) => other
    */


case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
    def contains(x: Int): Boolean = 
        if x < elem then left.contains(x)
        else if x > elem then right.contains(x)
        else true
    def incl(x: Int): IntSet = 
        if x < elem then NonEmpty(elem, left.incl(x), right)
        else if x > elem then NonEmpty(elem, left, right.incl(x))
        else this // already exists => does not need to include

    def union(other: IntSet): IntSet = 
        left.union(right.union(other)).incl(elem)
        //this.union(left.union(right.union(other)))
    
    def union2(other: IntSet): IntSet =
        left.union2(right.union2(other))//.incl(elem)

NonEmpty(0, Empty, Empty).incl(1).contains(1)

val intSet:IntSet = NonEmpty(0, Empty, Empty).incl(1).incl(2)
val intSet2:IntSet = NonEmpty(2, Empty, Empty).incl(3).incl(4)
val unionResult = NonEmpty(0, Empty, Empty).incl(1).incl(2).incl(3).incl(4)

intSet.union(Empty) == intSet
intSet.union(intSet2)
intSet.union2(intSet2)
intSet.union(intSet2).contains(4)
intSet.union(intSet2).contains(5)
intSet.union(intSet2).contains(6)

NonEmpty(0, Empty, Empty).union(NonEmpty(0, Empty, Empty).incl(1))
NonEmpty(0, Empty, Empty).union2(NonEmpty(0, Empty, Empty).incl(1))
NonEmpty(0, Empty, Empty).union2(NonEmpty(0, Empty, Empty))

LazyList(1,2,3)
LazyList(1,2,3)(1)

def lazyRange(lo: Int, hi: Int): LazyList[Int] = 
    if lo >= hi then LazyList.empty
    else LazyList.cons(lo, lazyRange(lo + 1, hi))

def listRange(lo: Int, hi: Int): List[Int] = 
    if lo >= hi then Nil
    else lo :: listRange(lo + 1, hi)

LazyList.range(10, 100)

LazyList(1, 2) #:: LazyList(1)

LazyList(1, 2).take(2).toList

/*
    val: 선언되었을때 evaluate 함
    lazy val: 호출되었을때 한번 evaluate하고 기억함
    def: 호출 할때마다 evaluate 함
*/

val x = {println("x"); 1}
lazy val y = {println("y"); 2}
def z = {println("z"); 3}

x + y + z + x + y + z //yzz

def from(n: Int): LazyList[Int] = n #:: from(n+1)

val nats = from(0)
nats.take(10)
nats.take(10).toList

def sieve(s: LazyList[Int]): LazyList[Int] = 
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))

val primes = sieve(from(2))
primes.take(10).toList

def sqrtSeq(x: Double): LazyList[Double] = 
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: LazyList[Double] = 1 #:: guesses.map(improve)
    guesses
