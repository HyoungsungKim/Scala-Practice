trait Generator[T]:
    def generate(): T
    def map[S](f:T => S): Generator[S] = new Generator[S]:
        def generate():S = f(Generator.this.generate())
    
    def flatMap[S](f:T => Generator[S]): Generator[S] = new Generator[S]:
        def generate():S = f(Generator.this.generate()).generate()
        
val ls = new Generator[List[Int]]:
    def generate(): List[Int] = 
        List(1,2,3)

ls.generate()

val integer:Generator[Int] = new Generator[Int]:
    def generate():Int = 
        val rand = java.util.Random()
        rand.nextInt()

integer.generate()
val boolean = for x <- integer yield x > 0
boolean.generate()
val boolean2 = integer.map(x => x>0)
boolean2.generate()

def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = 
    t.flatMap(x => u.map(y => (x, y)))

val p = pairs(integer, boolean)
p.generate()

def lists: Generator[List[Int]] = 
    for
        isEmpty <- boolean
        list <- if isEmpty then emptyLists else nonEmptyLists
    yield list

def single[T](x: T): Generator[T] = new Generator[T]:
    def generate() = x

def emptyLists = single(Nil)

def nonEmptyLists = 
    for
        head <- integer
        tail <- lists
    yield head :: tail

lists.generate()

enum Size:
    case Small, Medium, Large


enum Color(val rgb: Int):
  case Red   extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue  extends Color(0x0000FF)

Color.Red.rgb

enum Tree:
    case Inner(left: Tree, right:Tree)
    case Leaf(x: Int)

val a = Tree.Inner

def trees:Generator[Tree] =
    for
        isLeaf <- boolean
        tree <- if isLeaf then leafs else inners
    yield
        tree

def leafs = 
    for x <- integer yield Tree.Leaf(x)

def inners = 
    for
        x <- trees
        y <- trees
    yield Tree.Inner(x, y)

trees.generate()