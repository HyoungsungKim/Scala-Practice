package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal( b() * b() - 4 * a() * c() )
    

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal{
      val del = delta()
      if del < 0 then Set()
      else
        val sol1 = (-1 * b() - math.sqrt(del)) / (2 * a())
        val sol2 = (-1 * b() + math.sqrt(del)) / (2 * a())
        Set(sol1, sol2)
    }
    //Signal[Set[Double]](Set(sol1, sol2))