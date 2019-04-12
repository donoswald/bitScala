package donmiguel

import donmiguel.FiniteFieldElement

class FiniteFieldElementSpec extends UnitSpec {


  it should "support equality" in {
    var a = new FiniteFieldElement(2, 31)
    var b = new FiniteFieldElement(2, 31)
    var c = new FiniteFieldElement(15, 31)
    assertResult(true) {
      a == b
    }
    assertResult(true) {
      a != c
    }
    assertResult(false) {
      a != b
    }

  }

  it should "support addition" in {
    assert(new FiniteFieldElement(2, 31) + new FiniteFieldElement(15, 31) == new FiniteFieldElement(17, 31))
    assert(new FiniteFieldElement(17, 31) + new FiniteFieldElement(21, 31) == new FiniteFieldElement(7, 31))
  }

  it should "support substraction" in {
    assert(new FiniteFieldElement(29, 31) - new FiniteFieldElement(4, 31) == new FiniteFieldElement(25, 31))
    assert(new FiniteFieldElement(15, 31) - new FiniteFieldElement(30, 31) == new FiniteFieldElement(16, 31))
  }

  it should "support multiplication" in {
    assert(new FiniteFieldElement(24, 31) * new FiniteFieldElement(19, 31) == new FiniteFieldElement(22, 31))
  }

  it should "support multiplication with a coefficient" in {
    var a = new FiniteFieldElement(24, 31)
    assert(a * 2 == a + a)
  }

  it should "support exponentiation" in {
    assert(new FiniteFieldElement(17, 31) ** 3 == new FiniteFieldElement(15, 31))
    assert(new FiniteFieldElement(5, 31) ** 5 * new FiniteFieldElement(18, 31) == new FiniteFieldElement(16, 31))
    assert(new FiniteFieldElement(17, 31) ** (-3) == new FiniteFieldElement(29, 31))
    assert(new FiniteFieldElement(4, 31) ** (-4) * new FiniteFieldElement(11, 31) == new FiniteFieldElement(13, 31))
  }

  it should "support division" in {
    assert(new FiniteFieldElement(3, 31) / new FiniteFieldElement(24, 31) == new FiniteFieldElement(4, 31))
  }

  it should "support arithmetic to check ec" in {
    var prime = 223
    var a = new FiniteFieldElement(0, 223)
    var b = new FiniteFieldElement(7, 223)

    var validPoints = Array.ofDim[Int](3, 2)
    validPoints(0)(0) = 192
    validPoints(0)(1) = 105
    validPoints(1)(0) = 17
    validPoints(1)(1) = 56
    validPoints(2)(0) = 1
    validPoints(2)(1) = 193

    for (pair <- validPoints) {
      var x = new FiniteFieldElement(pair(0), prime)
      var y = new FiniteFieldElement(pair(1), prime)
      assert(y ** 2 == x ** 3 + a * x + b, "the point is not on the curve")
    }

    var invalidPoints = Array.ofDim[Int](3, 2)
    invalidPoints(0)(0) = 200
    invalidPoints(0)(1) = 119
    invalidPoints(1)(0) = 42
    invalidPoints(1)(1) = 99
    for (pair <- invalidPoints) {
      var x = new FiniteFieldElement(pair(0), prime)
      var y = new FiniteFieldElement(pair(1), prime)
      assertResult(false)  {
        y ** 2 == x ** 3 + a * x + b
      }
    }

  }


}
