package calculator

import Math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      pow(b(), 2) - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val d = delta()
      if(d < 0) Set()
      else {
        val bVal = b()
        val aVal = a()

        Set(
          (-bVal + sqrt(d)) / (2 * aVal),
          (-bVal - sqrt(d)) / (2 * aVal)
        )
      }
    }
  }
}
