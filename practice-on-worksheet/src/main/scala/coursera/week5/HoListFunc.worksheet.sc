def scaleList(xs: List[Double], factor: Double): List[Double] = xs match
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)

scaleList(List(1,2,3), 2)

extension [T](xs: List[T])
    def myMap[U](f: T => U): List[U] = xs match
        case Nil => Nil//xs
        case x :: xs => f(x) :: xs.myMap(f)

List(1,2,3).myMap( x => x*2)
//List().myMap( x => x*2)
//List().map( x => x * 2)

def squareList(xs: List[Int]): List[Int] = xs match
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)

squareList(List(1, 2, 3))

def squareList2(xs: List[Int]): List[Int] = 
    xs.map(x => x*x)

squareList2(List(1, 2, 3))

def posElems(xs: List[Int]): List[Int] = xs match
    case Nil => xs
    case y :: ys => if y > 0 then y :: posElems(ys) else posElems(ys)

List(1,2,3,4,5,6,7,8).partition(x => x%2 != 0)
List(1,2,3,4,5,6,7,8).span(x => x%2 != 0)
List(1,2) :: List(List(3, 4))

def pack[T](xs: List[T]): List[List[T]] = xs match
    case Nil      => Nil
    case x :: xs1 => 
        val (a, b) = xs1.partition(elem => elem == x)
        (x :: a) :: pack(b)

List("a","a","a","b","b","b","c","c","c").filter(elem => elem == "a")
List("a","a","a","b","b","b").partition(elem => elem == "a")
pack(List("a","a","a","b","b","b"))

def consecutivePack[T](xs: List[T]): List[List[T]] = xs match
    case Nil      => Nil
    case x :: xs1 => 
        val (a, b) = xs1.span(elem => elem == x)
        (x :: a) :: pack(b)

List(1,2,3).span(elem => elem == 0)
consecutivePack(List("a","a","a","b","b","b","a"))

List(Tuple2(1,2), Tuple2(3,4))

def encode[T](xs: List[T]): List[(T, Int)] = //xs match
    /*
    case Nil      => Nil
    case x :: xs1 => 
        val (a, b) = xs1.partition(elem => elem == x)
        Tuple2(a.head, (x :: a).length) :: encode(b)
    */
    consecutivePack(xs).map(x => (x.head, x.length))

encode(List("a","a","a","b","b","b","a"))