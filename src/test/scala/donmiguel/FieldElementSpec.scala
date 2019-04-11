package donmiguel

class FieldElementSpec extends UnitSpec {


  it should "support equality" in {
    var a = new FieldElement(2, 31)
    var b = new FieldElement(2, 31)
    var c = new FieldElement(15, 31)
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
    assert(new FieldElement(2, 31) + new FieldElement(15, 31) == new FieldElement(17, 31))
    assert(new FieldElement(17, 31) + new FieldElement(21, 31) == new FieldElement(7, 31))
  }

  it should "support substraction" in {
    assert(new FieldElement(29, 31) - new FieldElement(4, 31) == new FieldElement(25, 31))
    assert(new FieldElement(15, 31) - new FieldElement(30, 31) == new FieldElement(16, 31))
  }

  it should "support multiplication" in {
    assert(new FieldElement(24, 31) * new FieldElement(19, 31) == new FieldElement(22, 31))
  }

  it should "support multiplication with a coefficient" in {
    var a = new FieldElement(24, 31)
    assert(a * 2 == a + a)
  }

  it should "support exponentiation" in {
    assert(new FieldElement(17, 31) ** 3 == new FieldElement(15, 31))
    assert(new FieldElement(5, 31) ** 5 * new FieldElement(18, 31) == new FieldElement(16, 31))
    assert(new FieldElement(17, 31) ** (-3) == new FieldElement(29, 31))
    assert(new FieldElement(4, 31) ** (-4) * new FieldElement(11, 31) == new FieldElement(13, 31))
  }

  it should "support division" in {
    assert(new FieldElement(3, 31) / new FieldElement(24, 31) == new FieldElement(4, 31))
  }


}
