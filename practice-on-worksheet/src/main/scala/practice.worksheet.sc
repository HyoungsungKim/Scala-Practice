def factorial(x: BigInt): BigInt = 
    if x == 0 then 1 else x * factorial(x - 1)

factorial(5)

def m(args: String*) = 
    var i = 0
    while i < args.length do
        println(args(i))
        i += 1

m("Scala is fun")

val greetingStrings: Array[String] = new Array[String](3)
greetingStrings

for str <- greetingStrings do
    println(str)

greetingStrings.update(0, "Hello")

println(greetingStrings(0))
println(greetingStrings(1))

val oneTwo = List(1, 2)
val threeFour = List(3,4)
val oneTwoThreeFour = oneTwo  ::: threeFour

val list:List[Int] = List(1,2,3,4,5)

list.filter((elem:Int) => elem == 2)

val pair = (1,2)
pair._1
pair._2
pair(0)

Set(2,1,3)

import scala.collection.immutable.HashSet
HashSet(2,1,3)

val mapTest:Map[Int, Int] = Map(2->3, 1->2)

import scala.collection.immutable.HashMap
val hashMapTest:HashMap[Int, Int] = HashMap(2->3, 1->2)

val adjectives = List("One", "Two", "Red", "Blue")
val nouns = 
    for adj <- adjectives yield
        adj + " Fish"

class ChecksumAccmulator:
    private var sum = 0
    def add(b: Byte): Unit = 
        sum += b

    def checksum(): Int = 
        return ~(sum & 0xFF) + 1

val acc = new ChecksumAccmulator
acc

case class Person(name:String, age:Int):
    def appendToName(suffix: String): Person = 
        Person(s"$name$suffix", age)

object Person:
    def apply(name: String, age: Int): Person = 
        val capitalizeName = 
            if !name.isEmpty then
                val firstChar = name.charAt(0).toUpper
                val restOfName = name.substring(1)
                s"$firstChar$restOfName"
            else throw new IllegalArgumentException("Empty name")
        
        new Person(capitalizeName, age)

val q = Person("sally", 39)

val fullName = q.appendToName(" Smith")
Person.apply(fullName.name, fullName.age)
