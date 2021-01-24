object Bench extends App {
  val filename = args(0)
  val data = scala.io.Source
    .fromFile(new java.io.File(filename))
    .getLines
    .toVector
    .grouped(2)
    .toVector
    .map { group =>
      val a = group(0).drop(1)
      val b = group(1).drop(1)
      (a, b)
    }
  // warmup
  0 until 10 foreach { _ =>
    data.foreach {
      case (a, b) =>
        pairwisealignment.wfa.WFA.globalAffineAlignment(
          a,
          b,
          x = 4,
          o = 6,
          e = 2
        )
    }
  }
  // real
  var t = 0L
  data.foreach {
    case (a, b) =>
      val t1 = System.nanoTime
      pairwisealignment.wfa.WFA.globalAffineAlignment(a, b, x = 4, o = 6, e = 2)
      t += (System.nanoTime - t1)
  }
  println(s"${data.size} pairs")
  println(s"${t * 1e-6}ms")
}
