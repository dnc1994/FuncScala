package calculator

object Polynomial {
    def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
        Signal(b() * b() - 4.0 * a() * c())
    }

    def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
        Signal(
            if (delta() < 0.0) Set()
            else Set(
                (-1.0 * b() + math.sqrt(delta())) / 2.0 / a(),
                (-1.0 * b() - math.sqrt(delta())) / 2.0 / a()
            )
        )
    }

}
