trait Generator[T]:
    def generator(): T

extension [T, S](g: Generator[T])
    def map(f:T => S) = new Generator[S]:
        def generator() = f(g.generator())

    def flatMap(f: T => Generator[S]) = new Generator[S]:
        def generator() = f(g.generator()).generator()

val integer = new Generator[Int]:
    def generator():Int = 
        val rand = java.util.Random()
        rand.nextInt()

val gen:Generator[Int] = integer
gen.generator()

object IntGen extends Generator[Int]:
    def generator():Int = 
        val rand = java.util.Random()
        rand.nextInt()

val bool = new Generator[Boolean]:
    def generator():Boolean = 
        IntGen.generator() > 0

IntGen.generator()
bool.generator()


gen.flatMap(g => gen).generator()
gen.flatMap(g => bool).generator()

val b:Boolean = false
// int에서 boolean type 값을 받아 boolean generator 생성
gen.map(i => b).generator()
gen.map(i => i > 0).generator()

def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = 
    t.flatMap(x => u.map(y => (x, y)))

val p = pairs(gen, bool)
p.generator()