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

    var arr = Array.ofDim[Int](3, 6)

    arr(0)(0) = 192
    arr(0)(1) = 105
    arr(0)(2) = 17
    arr(0)(3) = 56
    arr(0)(4) = 170
    arr(0)(5) = 142
    arr(1)(0) = 47
    arr(1)(1) = 71
    arr(1)(2) = 117
    arr(1)(3) = 141
    arr(1)(4) = 60
    arr(1)(5) = 139
    arr(2)(0) = 143
    arr(2)(1) = 98
    arr(2)(2) = 76
    arr(2)(3) = 66
    arr(2)(4) = 47
    arr(2)(5) = 71

    for (row <- arr) {
      var x1 = new FiniteFieldElement(row(0), prime)
      var y1 = new FiniteFieldElement(row(1), prime)
      var p1 = new Point(x1, y1, a, b)
      var x2 = new FiniteFieldElement(row(2), prime)
      var y2 = new FiniteFieldElement(row(3), prime)
      var p2 = new Point(x2, y2, a, b)
      var x3 = new FiniteFieldElement(row(4), prime)
      var y3 = new FiniteFieldElement(row(5), prime)
      var p3 = new Point(x3, y3, a, b)

      assert(p1 + p2 == p3)
    }
  }

  it should "support multiplication with coefficient" in {
    var prime = 223
    var a = new FiniteFieldElement(0, prime)
    var b = new FiniteFieldElement(7, prime)

    var arr = Array.ofDim[Int](6, 5)

    arr(0)(0) = 2
    arr(0)(1) = 192
    arr(0)(2) = 105
    arr(0)(3) = 49
    arr(0)(4) = 71

    arr(1)(0) = 2
    arr(1)(1) = 143
    arr(1)(2) = 98
    arr(1)(3) = 64
    arr(1)(4) = 168
    arr(2)(0) = 2
    arr(2)(1) = 47
    arr(2)(2) = 71
    arr(2)(3) = 36
    arr(2)(4) = 111
    arr(3)(0) = 4
    arr(3)(1) = 47
    arr(3)(2) = 71
    arr(3)(3) = 194
    arr(3)(4) = 51
    arr(4)(0) = 8
    arr(4)(1) = 47
    arr(4)(2) = 71
    arr(4)(3) = 116
    arr(4)(4) = 55
    arr(5)(0) = 21
    arr(5)(1) = 47
    arr(5)(2) = 71
    arr(5)(3) = -1
    arr(5)(4) = -1
    for (row <- arr) {
      var s = row(0)
      var x1 = new FiniteFieldElement(row(1), prime)
      var y1 = new FiniteFieldElement(row(2), prime)
      var p1 = new Point(x1, y1, a, b)

      var p2: Point = null
      if (row(3) == -1) {
        p2 = new Point(ElementNone, ElementNone, a, b)
      } else {
        var x2 = new FiniteFieldElement(row(3), prime)
        var y2 = new FiniteFieldElement(row(4), prime)
        p2 = new Point(x2, y2, a, b)
      }
      assert(p1 * s == p2)
    }
  }

}
