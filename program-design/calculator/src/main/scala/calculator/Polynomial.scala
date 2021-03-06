package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val B = b()
    math.pow(B, 2) - 4 * (a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    Set((-b() + math.sqrt(delta())) / (2 * a()),
      (-b() - math.sqrt(delta())) / (2 * a()))
    }
}
