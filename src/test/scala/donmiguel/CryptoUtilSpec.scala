package donmiguel

class CryptoUtilSpec extends UnitSpec {

  it should "test 10" in {
    val a = CryptoUtil.toVarint(10) // with widening conversion
    assertEquals(BigInt.apply(a), BigInt.apply(10))
    assertEquals(10, CryptoUtil.fromVarint(a))
  }

  it should "test shorts" in {
    val a = CryptoUtil.toVarint(64000)
    assertEquals(BigInt.apply(a), BigInt.apply("-196358"))
    assertEquals(64000, CryptoUtil.fromVarint(a))
  }

  it should "test ffff" in {
    val a = CryptoUtil.toVarint(0xFFFFL)
    assertEquals(BigInt.apply(a), BigInt.apply("-131073"))
    assertEquals(0xFFFFL, CryptoUtil.fromVarint(a))
  }

  it should "test ints" in {
    val a = CryptoUtil.toVarint(0xAABBCCDDL)
    assertEquals(BigInt.apply(a), BigInt.apply("-4868752470"))
    assertEquals(0xAABBCCDDL, CryptoUtil.fromVarint(a))
  }

  it should "test ffffffff" in {
    val a = CryptoUtil.toVarint(0xFFFFFFFFL)
    assertEquals(BigInt.apply(a), BigInt.apply("-4294967297"))
    assertEquals(0xFFFFFFFFL, CryptoUtil.fromVarint(a))
  }

  it should "test long" in {
    val a = CryptoUtil.toVarint(0xCAFEBABEDEADBEEFL)
    assertEquals(BigInt.apply(a), BigInt.apply("-1171307680875479350"))
    assertEquals(0xCAFEBABEDEADBEEFL, CryptoUtil.fromVarint(a))
  }

  def assertEquals(x: Any, y: Any) = {
    assert(x == y)
  }

}
