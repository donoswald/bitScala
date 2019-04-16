package donmiguel

class PointSpec extends UnitSpec {

  it should "assert equality" in {
    var a = new Point(int(3), int(-7), int(5), int(7))
    var b = new Point(int(18), int(77), int(5), int(7))
    assertResult(true) {
      a != b
    }
    assertResult(false) {
      a != a
    }
  }

  it should "check if on curve" in {
    a[AssertionError] should be thrownBy {
      new Point(int(-2), int(4), int(5), int(7))
    }
    new Point(int(3), int(-7), int(5), int(7))
    new Point(int(18), int(77), int(5), int(7))
  }

  it should "compare infinity" in {
    assert(new Point(ElementNone, ElementNone, int(5), int(7)) == new Point(ElementNone, ElementNone, int(5), int(7)))
  }

  it should "support addition 1" in {
    var a = new Point(ElementNone, ElementNone, int(5), int(7))
    var b = new Point(int(2), int(5), int(5), int(7))
    var c = new Point(int(2), int(-5), int(5), int(7))
    assert(a + b == b)
    assert(b + a == b)
    assert(b + c == a)
  }

  it should "support addition 2" in {
    var a = new Point(int(3), int(7), int(5), int(7))
    var b = new Point(int(-1), int(-1), int(5), int(7))
    assert(a + b == new Point(int(2), int(-5), int(5), int(7)))

  }

  it should "support addition 3" in {
    var a = new Point(int(-1), int(1), int(5), int(7))
    assert(a + a == new Point(int(18), int(-77), int(5), int(7)))
  }

  private def int(x: Int): IntegerElement = {
    return new IntegerElement(x)
  }

  it should "support addition on FiniteFields" in {
    var prime = 223
    var a = new FiniteFieldElement(0, prime)
    var b = new FiniteFieldElement(7, prime)

    var arr = Array(
      (192, 105, 17, 56, 170, 142),
      (47, 71, 117, 141, 60, 139),
      (143, 98, 76, 66, 47, 71)
    )

    for (row <- arr) {
      var x1 = new FiniteFieldElement(row._1, prime)
      var y1 = new FiniteFieldElement(row._2, prime)
      var p1 = new Point(x1, y1, a, b)
      var x2 = new FiniteFieldElement(row._3, prime)
      var y2 = new FiniteFieldElement(row._4, prime)
      var p2 = new Point(x2, y2, a, b)
      var x3 = new FiniteFieldElement(row._5, prime)
      var y3 = new FiniteFieldElement(row._6, prime)
      var p3 = new Point(x3, y3, a, b)

      assert(p1 + p2 == p3)
    }
  }

  it should "support multiplication with coefficient" in {
    var prime = 223
    var a = new FiniteFieldElement(0, prime)
    var b = new FiniteFieldElement(7, prime)

    var arr = Array(
      (2,192,105,49,71),
      (2,143,98,64,168),
      (2,47,71,36,111),
      (8,47,71,116,55),
      (21,47,71,-1,-1)
    )

    for (row <- arr) {
      var s = row._1
      var x1 = new FiniteFieldElement(row._2, prime)
      var y1 = new FiniteFieldElement(row._3, prime)
      var p1 = new Point(x1, y1, a, b)

      var p2: Point = null
      if (row._4 == -1) {
        p2 = new Point(ElementNone, ElementNone, a, b)
      } else {
        var x2 = new FiniteFieldElement(row._4, prime)
        var y2 = new FiniteFieldElement(row._5, prime)
        p2 = new Point(x2, y2, a, b)
      }
      assert(p1 * s == p2)
    }
  }

}
