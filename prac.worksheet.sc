def product(f:Int => Int)(a:Int, b:Int) =
    if a > b then 1 else f(a) * product(f)(a+1, b)

product(x => x*x)(1,5)

def fact(n: Int) = product(x => x)(1, n)

def mapReduce(f:Int => Int, combine:(Int, Int) => Int, zero:Int)(a:Int, b:Int):Int = 
    def recursion(a: Int):Int = 
        if a > b then zero
        else combine(f(a), recursion(a+1))
    recursion(a)

def sum(f:Int => Int) = mapReduce(f, (x,y) => x+y, 0)