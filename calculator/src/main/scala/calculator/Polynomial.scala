package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    
    Signal {
      val aSig = a()
      val bSig = b()
      val deltaSig = delta()

      if (deltaSig < 0) Set()
      else if (aSig == 0 && bSig == 0) Set()
      else if (aSig == 0) Set(-c() / bSig)
      else {
        Set(scala.math.sqrt(deltaSig), -scala.math.sqrt(deltaSig))
          .map(x => (-bSig + x) / (2 * aSig))
      }
    }
  }
}
