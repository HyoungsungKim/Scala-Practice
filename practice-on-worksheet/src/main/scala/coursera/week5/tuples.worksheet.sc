// Merge sort
def merge(xs: List[Int], ys: List[Int]):List[Int] = (xs, ys) match
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xs1, y:: ys1) =>
        if x < y then  x :: merge(xs1, ys)
        else y :: merge(xs, ys1)


extension [T](xs: List[T])
    def splitAt(n: Int) = (xs.take(n), xs.drop(n))

val a = List(1,2,3,4,5,6)
val b = List(1,2,3,4,5)

a.splitAt(3)
b.splitAt(3)

a.drop(3)
b.drop(3)

// drop(n) : 앞에서부터 n개 제거한것
// take(n) : 앞에서부터 n개 선택한것

val c:Tuple3[Int, String, Boolean] =
    (1, "hello", true)

val d = (1, "hello", true)

d._1


val (q, w, e) = c