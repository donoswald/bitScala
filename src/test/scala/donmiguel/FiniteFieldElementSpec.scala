package donmiguel

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


}
