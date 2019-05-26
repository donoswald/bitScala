package donmiguel.util

import donmiguel.UnitSpec

class VarIntSpec extends UnitSpec {

  it should "test 10" in {
    val a = VarInt.toVarint(10) // with widening conversion
    assertEquals(BigInt.apply(a), BigInt.apply(10))
    assertEquals(10, VarInt.fromVarint(a.iterator))
  }

  it should "test shorts" in {
    val a = VarInt.toVarint(64000)
    assertEquals(BigInt.apply(a), BigInt.apply("-196358"))
    assertEquals(64000, VarInt.fromVarint(a.iterator))
  }

  it should "test ffff" in {
    val a = VarInt.toVarint(0xFFFFL)
    assertEquals(BigInt.apply(a), BigInt.apply("-131073"))
    assertEquals(0xFFFFL, VarInt.fromVarint(a.iterator))
  }

  it should "test ints" in {
    val a = VarInt.toVarint(0xAABBCCDDL)
    assertEquals(BigInt.apply(a), BigInt.apply("-4868752470"))
    assertEquals(0xAABBCCDDL, VarInt.fromVarint(a.iterator))
  }

  it should "test ffffffff" in {
    val a = VarInt.toVarint(0xFFFFFFFFL)
    assertEquals(BigInt.apply(a), BigInt.apply("-4294967297"))
    assertEquals(0xFFFFFFFFL, VarInt.fromVarint(a.iterator))
  }

  it should "test long" in {
    val a = VarInt.toVarint(0xCAFEBABEDEADBEEFL)
    assertEquals(BigInt.apply(a), BigInt.apply("-1171307680875479350"))
    assertEquals(0xCAFEBABEDEADBEEFL, VarInt.fromVarint(a.iterator))
  }

  it should "515" in {
    assert(515L == VarInt.fromVarint(CryptoUtil.hexToBytes("fd0302").iterator))
  }


  def assertEquals(x: Any, y: Any) = {
    assert(x == y)
  }


}
