def concat[T](xs: List[T], ys: List[T]): List[T] = 
    xs.foldRight(ys)(_ :: _)

concat(List(1, 2, 3), List(4, 5, 6))

def reverse[T](xs: List[T]): List[T] =
    //parameter: (initial expression, expression from object)
    xs.foldLeft(List[T]())((a, b) => b :: a)
    
1 :: List(1)

reverse(List(1,2,3))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    xs.foldRight(List[U]())((elem:T, initList) => f(elem) :: initList)

mapFun(List(1,2,3,4,5,6), (elem => elem % 2 == 0))

def lengthFun[T](xs: List[T]): Int = 
    xs.foldRight(0)((elem, init) => init + 1)

lengthFun(List(1,2,3))
lengthFun(List())
