package donmiguel

class ElementSpec extends UnitSpec {

  it should "support == " in {
    assert(ElementNone==ElementNone)
  }

  it should "support != " in {
    assert(ElementNone != None)
  }
}
