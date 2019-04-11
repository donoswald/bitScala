package donmiguel

class PointSpec extends UnitSpec {

  it should "assert equality" in {
    var a = new Point(Some(3), Some(-7), 5, 7)
    var b = new Point(Some(18), Some(77), 5, 7)
    assertResult(true) {
      a != b
    }
    assertResult(false) {
      a != a
    }
  }

  it should "check if on curve" in {
    a[AssertionError] should be thrownBy {
      new Point(Some(-2), Some(4), 5, 7)
    }
    new Point(Some(3), Some(-7), 5, 7)
    new Point(Some(18), Some(77), 5, 7)
  }

  it should "support addition 1" in {
    var a = new Point(None, None, 5, 7)
    var b = new Point(Some(2), Some(5), 5, 7)
    var c = new Point(Some(2), Some(-5), 5, 7)
    assert(a + b == b)
    assert(b + a == b)
    assert(b + c == a)
  }

  it should "support addition 2" in {
    var a = new Point(Some(3), Some(7), 5, 7)
    var b = new Point(Some(-1), Some(-1), 5, 7)
    assert(a + b == new Point(Some(2), Some(-5), 5, 7))

  }

  it should "support addition 3" in {
    var a = new Point(Some(-1), Some(1), 5, 7)
    assert(a + a == new Point(Some(18), Some(-77), 5, 7))
  }

}
