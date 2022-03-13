val fruit = List("apples", "oranges", "pears")
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

Nil == List()

"apple" :: ("orange" :: Nil)
"apple" :: "orange" :: Nil

//1 :: 2 :: List(3, 4) :: 5

// Sorting List
def insert(src:Int, des:List[Int]):List[Int] = des match 
    case List() => List(src)
    // head y and tail ys
    case y :: ys => 
        if src < y then src :: des else y :: insert(src, ys)

def isort(xs: List[Int]): List[Int] = xs match
    case List() => List()
    case y :: ys => insert(y, isort(ys))

isort(List(7, 3, 9, 2))
isort(1 :: List(2,3,4))

var a = 1 :: 2 :: 3 :: Nil
a = 2 :: 3 :: Nil
a
