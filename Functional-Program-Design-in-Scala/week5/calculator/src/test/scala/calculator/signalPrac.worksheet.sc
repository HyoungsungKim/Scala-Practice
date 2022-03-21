trait Signal[+T]:
  def apply()(using caller: Signal.Caller): T
  def currentValue: T

object Signal:
  abstract class AbstractSignal[+T] extends Signal[T]:
    private var _currentValue: T = compiletime.uninitialized
    private var observers: Set[Caller] = Set()

    protected def eval: Caller => T

    protected def computeValue(): Unit =
      val newValue = eval(this)
      val observeChange = observers.nonEmpty && newValue != _currentValue
      _currentValue = newValue
      if observeChange then
        val obs = observers
        observers = Set()
        obs.foreach(_.computeValue())

    def apply()(using caller: Caller): T =
      observers += caller
      assert(!caller.observers.contains(this), "cyclic signal definition")
      _currentValue

    def currentValue: T = _currentValue
  end AbstractSignal

  def apply[T](expr: Caller ?=> T): Signal[T] =
    new AbstractSignal[T]:
      protected val eval = expr(using _)
      computeValue()

  @annotation.implicitNotFound("You can only observe a Signal value within a Signal definition like in `Signal{ ... }`. If you want to just read the current value, use the method `currentValue`.")
  opaque type Caller = AbstractSignal[?]

  class Var[T](expr: Signal.Caller ?=> T) extends Signal.AbstractSignal[T]:
    protected var eval: Signal.Caller => T = expr(using _)
    computeValue()

    def update(expr: Signal.Caller ?=> T): Unit =
      eval = expr(using _)
      computeValue()
  end Var

end Signal

class BankAccount:
    def balance: Signal[Int] = myBalance

    private val myBalance = Signal.Var[Int](0)

    def deposit(amount: Int): Unit = 
        if amount > 0 then
            val b = myBalance.currentValue
            myBalance() = b + amount

    def withdraw(amount: Int): Int =
        if 0 < amount && amount <= balance.currentValue then
            val b = myBalance.currentValue
            myBalance() = b - amount
            myBalance.currentValue
        else assert(false)

end BankAccount

def consolidated(accts: List[BankAccount]): Signal[Int] = 
    Signal(accts.map(_.balance()).sum)

val a = BankAccount()
val b = BankAccount()
val c = consolidated(List(a, b))
c.currentValue
a.deposit(10)
c.currentValue
b.deposit(20)

enum temp:
  case Val1(a: Int)

val v = temp.Val1(1)
v
//v.id

Map(1 -> 2, 2->3)
