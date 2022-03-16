val a:List[Int] = List(1,2,3)
a.updated(1,0)
a
a.indexOf(0)
a.indexOf(1)
a.contains(3)
a.contains(4)

a
a.init
a.take(0)
a.take(1)

def myLast[T](xs: List[T]):T = xs match
    case List() => throw Error("last of empty list")
    case List(x) => x
    case y :: ys => myLast(ys)
end myLast

myLast(List(1,2,3))
List(1, 2, 3).last

def myInit[T](xs: List[T]):List[T] = xs match
    case List() => throw Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: myInit(ys)
end myInit

List() ++ List(1,2)

extension [T](xs: List[T])
    //def ++ (ys: List[T]): List[T] = xs ::: ys
    def ++ (ys: List[T]): List[T] = xs match
        case Nil => ys
        case List(x) => x :: ys
        case head :: tail => head :: (tail ++ ys)

    def reverse: List[T] = xs match
        case Nil => Nil
        case y :: ys => ys.reverse ++ List(y)
    
    
List().++(List(1,2))
Nil.++(List(1,2,3))

Nil ::: List(1,2,3)

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match
    case Nil => Nil
    case y::ys => 
        if n == 0 then ys
        else y :: removeAt(n - 1, ys)

Nil == List()
removeAt(2, List(0, 1, 2, 3, 4))

List(List(1,2,3), List(4,5,6)).head

def flatten(xs: Any): List[Any] = xs match
    case Nil => Nil
    case y :: ys => flatten(y) ++ flatten(ys)
    case _ => List(xs)

flatten(List(List(1,2,3), List(4,5,6)))
flatten(List(1, List(2, 3, List(4, 5, 6)), List(7, 8, 9)))

(List(List(1,2,3), List(4,5,6))).flatten