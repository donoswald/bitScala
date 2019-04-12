package donmiguel

class PointSpec extends UnitSpec {
  
  it should "assert equality" in {
    var a = new Point(Some(int(3)), Some(int(-7)), int(5), int(7))
    var b = new Point(Some(int(18)), Some(int(77)), int(5), int(7))
    assertResult(true) {
      a != b
    }
    assertResult(false) {
      a != a
    }
  }

  it should "check if on curve" in {
    a[AssertionError] should be thrownBy {
      new Point(Some(int(-2)), Some(int(4)),int( 5), int(7))
    }
    new Point(Some(int(3)), Some(int(-7)), int(5), int(7))
    new Point(Some(int(18)), Some(int(77)), int(5), int(7))
  }

  it should "support addition 1" in {
    var a = new Point(None, None, int(5), int(7))
    var b = new Point(Some(int(2)), Some(int(5)), int(5), int(7))
    var c = new Point(Some(int(2)), Some(int(-5)), int(5), int(7))
    assert(a + b == b)
    assert(b + a == b)
    assert(b + c == a)
  }

  it should "support addition 2" in {
    var a = new Point(Some(int(3)), Some(int(7)), int(5), int(7))
    var b = new Point(Some(int(-1)), Some(int(-1)), int(5), int(7))
    assert(a + b == new Point(Some(int(2)), Some(int(-5)), int(5), int(7)))

  }

  it should "support addition 3" in {
    var a = new Point(Some(int(-1)), Some(int(1)), int(5), int(7))
    assert(a + a == new Point(Some(int(18)), Some(int(-77)), int(5), int(7)))
  }
  private def int(x:Int):IntegerElement = {
    return new IntegerElement(x)
  }

}
