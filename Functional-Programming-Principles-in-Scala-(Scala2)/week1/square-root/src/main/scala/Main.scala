object Main extends App {
  def abs(x: Double) = if(x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double = {
    if(isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double) = 
    abs((guess - x/guess)*guess) < 1.0e-15

  def improve(guess: Double, x: Double): Double = {
    guess/2 + x/(2*guess) 
  }

  def sqrt(x: Double) = sqrtIter(1.0, x)

  println(sqrt(2)) 
  println(sqrt(1.0e-6))
  println(sqrt(1.0e20))
  println(sqrt(1.0e50))

  def factorial(n: Int, src:Int, dest: Int): Int = {
    if(src == n) src*dest else factorial(n, src+1, dest*src)
  }
  
  def factorial2(n: Int): Int = {
    def loop(src: Int, dst: Int): Int = {
      if (src == 0) dst else loop(src-1, src*dst)
    }

    loop(n, 1)
  }

  println(factorial(3, 1, 1))
  println(factorial(5, 1, 1))

  println(factorial2(3))
  println(factorial2(5))
}