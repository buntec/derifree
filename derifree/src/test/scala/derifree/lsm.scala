package derifree

class LsmSuite extends munit.FunSuite:

  test("speed of polynomial regression".ignore):
    val rows = List(
      (List(0.1, 2.1), 1.1),
      (List(0.3, 1.1), 0.3),
      (List(0.1, -0.3), 1.3),
      (List(3.1, 1.2), -3.1),
      (List(2.1, 4.2), 3.1),
      (List(3.4, -2.2), 3.7),
      (List(8.2, 1.5), 1.7),
      (List(3.1, -1.8), 2.9),
      (List(7.1, 3.2), 2.9)
    )
    var j = 0
    while (j < 1000) {
      val t1 = System.nanoTime()
      var i = 0
      var sum = 0.0
      while (i < 64000) {
        val lsm = Lsm.fromPoly(3)
        val est = lsm.toContValueEstimator(rows).toOption.get
        sum += est(IndexedSeq(0.4, 0.2))
        i += 1
      }
      val t2 = System.nanoTime()
      println(s"took ${(t2 - t1) * 1e-6} ms")
      j += 1
    }
