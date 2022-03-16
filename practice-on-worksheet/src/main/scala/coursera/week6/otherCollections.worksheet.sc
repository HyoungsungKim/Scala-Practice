val nums = Vector(1, 2, 3, 4)
val people = Vector("Bob", "james")

nums ++ nums
nums +: nums

1 until 5
1 to 5

nums.zip(nums)
nums.zip(nums).unzip

nums.zip(nums).map((x, y) => x * y).sum

nums.zip(nums).map(_*_).sum

def isPrime(n: Int): Boolean = 
    //(1 to n).map(n/_).length == 2
    (2 until n).forall(n%_ != 0)

isPrime(2)
isPrime(3)
isPrime(4)


(1 until 5).map(i => (1 until i).map(j => (i, j)))
(1 until 5).map(i => (1 until i).map(j => (i, j))).flatten

for (x, y) <- nums.zip(nums) yield x * y

for 
    x <- nums
    y <- nums
yield x * y

print(Set(3,1,2))
import scala.collection.immutable.HashSet
print(HashSet(3,1,2))

val m = Map(1 -> 2, 2 -> 2, 3->3)
m.map((x,y) => Map(y -> x))
m.get(1)
m.get(4)

m + (4->5)
m + (1->3)
m ++ Map(4 -> 5)
m

val fruit = List("apple", "pear", "orange", "pineapple")
fruit.groupBy(_.head)
m
m.withDefaultValue(10)(5)
m.withDefaultValue(10).get(5)
// m(10) => error
m(1)
m.get(1)

