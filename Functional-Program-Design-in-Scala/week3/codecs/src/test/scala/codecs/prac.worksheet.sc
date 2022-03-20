def maxList[T](elements: List[T])(using Ordering[T]): T = 
    elements match
        case List() => throw new IllegalArgumentException("empty list!")
        case List(x) => x
        case x :: rest =>
            val maxRest = maxList(rest)
            if summon[Ordering[T]].gt(x, maxRest) then x
            else maxRest

maxList(List(1,2,3))
summon[Ordering[Int]]
summon

trait JsonSerializer[T]:
    def serialize(o: T): String

    extension (a: T)
        def toJson: String = serialize(a)

object JsonSerializer:
    given stringSerializer: JsonSerializer[String] with
        def serialize(s: String) = s"\"$s\""

    given intSerializer: JsonSerializer[Int] with
        def serialize(n: Int) = n.toString

    given longSerializer: JsonSerializer[Long] with
        def serialize(n: Long) = n.toString

    given booleanSerializer: JsonSerializer[Boolean] with
        def serialize(b: Boolean) = b.toString

    given listSerializer[T](using JsonSerializer[T]): JsonSerializer[List[T]] with
        def serialize(ts: List[T]) = s"[${ts.map(t => t.toJson).mkString(", ")}]"

object ToJsonMethods:
    extension [T](a: T)(using jser: JsonSerializer[T])
        def toJson: String = jser.serialize(a)

import ToJsonMethods.*
"tennis".toJson
10.toJson
true.toJson
List(1,2,3).toJson

case class Address (
    street: String,
    city: String,
    state: String,
    zip: Int
)

object Address:
    given addressSerializer: JsonSerializer[Address] with
        def serialize(a: Address) = 
            import ToJsonMethods.{toJson as asJson}
            s"""|{
                |   "street": ${a.street.asJson}
                |   "city": ${a.city.asJson}
                |   "state": ${a.state.asJson}
                |   "zip": ${a.zip.asJson}
                |}
            """.stripMargin

object Phone:
    given phoneSerializer: JsonSerializer[Phone] with
        def serialize(p: Phone) = 
            import ToJsonMethods.{toJson as asJson}
            s"""|{
                |   "countryCode": ${p.countryCode.asJson},
                |   "phoneNumber": ${p.phoneNumber.asJson}
                |}""".stripMargin            

case class Phone(
    countryCode: Int,
    phoneNumber: Long
)


case class Contact (
    name: String,
    addresses: List[Address],
    phones: List[Phone]
)

object Contact:
    given contactSerializer: JsonSerializer[Contact] with
        def serialize(c: Contact) = 
            import ToJsonMethods.{toJson as asJson}
            s"""|{
                |   "name": ${c.name.asJson},
                |   "addresses": ${c.addresses.asJson},
                |   "phones": ${c.phones.asJson}
                |}""".stripMargin
    

case class AddressBook(contracts: List[Contact])
object AddressBook:
    given addressBookSerializer: JsonSerializer[AddressBook] with
        def serialize(a: AddressBook) = 
            import ToJsonMethods.{toJson as asJson}
            s"""|{
                |   "contracts": ${a.contracts.asJson}
                |}""".stripMargin
val addressBook =
    AddressBook(
        List(
        Contact(
            "Bob Smith",
        List(
            Address(
            "12345 Main Street",
            "San Francisco",
            "CA",
            94105
        ),
            Address(
                "500 State Street",
                "Los Angeles",
                "CA",
                90007
            )
        ),
        List(
            Phone(
                1,
                555888123
            ),
            Phone(
                49,
                555841332
            )
            )
        )
    )
)

addressBook.toJson

case class temp(var t:Int)

var a = temp(1)
val b = a

a == b
a.t = 2
a
a.t = 3
b

// call by name : for mutable state
def repeatUntil(command: => Unit)(condition: => Boolean): Unit = 
    command
    if !condition then repeatUntil(command)(condition)
    else()

var x = 0
var y = 2

repeatUntil {
    x = x + 1
    y = y * 2
} (x == 5)
y

val q:Map[Int, Int] = Map(1 -> 1, 2->2)
q(1)
q.get(1)