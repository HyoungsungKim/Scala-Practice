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

object TweetLength:
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] =
    Signal[Int](140 - tweetText.currentValue.length)

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] =
    if remainingCharsCount.currentValue >= 15 then Signal[String]("green")
    else if remainingCharsCount.currentValue >= 0 && remainingCharsCount.currentValue < 15 then Signal[String]("orange")
    else Signal[String]("red")

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int =
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if text.isEmpty then 0
    else
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)

val a = 1
a

object Polynomial:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    val bSquare = b.currentValue * b.currentValue
    val left = 4 * a.currentValue * c.currentValue

    Signal[Double](bSquare - left)

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    val sol1 = (-1 * b.currentValue - math.sqrt(delta.currentValue)) / (2 * a.currentValue)
    val sol2 = (-1 * b.currentValue + math.sqrt(delta.currentValue)) / (2 * a.currentValue)

    Signal[Set[Double]](Set(sol1, sol2))

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator:
 import Expr.*
  // https://www.reddit.com/r/scala/comments/gx3c3f/what_does_it_mean_that_there_is_a_case_argument/
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =     
    
    namedExpressions.map ( (key, value) => value.currentValue match
      case Literal(v) => (key, Signal[Double](v))
      case Expr.Ref(name) => (key, Signal[Double](eval(namedExpressions(name).currentValue, namedExpressions)))
      case Plus(a, b) => (key, Signal[Double](eval(a, namedExpressions) + eval(b, namedExpressions)))
      case Minus(a, b) => (key, Signal[Double](eval(a, namedExpressions) - eval(b, namedExpressions)))
      case Times(a, b) => (key, Signal[Double](eval(a, namedExpressions) * eval(b, namedExpressions)))
      case Divide(a, b) => (key, Signal[Double](eval(a, namedExpressions) / eval(b, namedExpressions)))
    )  

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double = expr match
      case Literal(v) => v
      case Ref(name) if references.contains(name) => (        
        eval(references(name).currentValue, references)
      )
      case _ => assert(false)

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }

Map('a' -> 2) - 'a'