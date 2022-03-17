val rand = java.util.Random()
rand.nextInt()

trait Generator[+T]:
    def generator():T

val integer = new Generator[Int]:
    def generator():Int = 
        val rand = java.util.Random()
        rand.nextInt()

integer.generator()

val boolean = new Generator[Boolean]:
    def generator(): Boolean = 
        integer.generator() > 0

boolean.generator()

val pairs = new Generator[(Int, Int)]:
    def generator(): (Int, Int) =
        (integer.generator(), integer.generator())

pairs.generator()






