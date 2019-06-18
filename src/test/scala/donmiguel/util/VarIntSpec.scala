package donmiguel.util

import donmiguel.UnitSpec

class VarIntSpec extends UnitSpec {

  it should "test 10" in {
    val a = new VarInt(10) // with widening conversion
    assertEquals(BigInt.apply(a.serialize()), BigInt.apply(10))
    assertEquals(10, VarInt.parse(a.serialize().iterator).value)
  }

  it should "test shorts" in {
    val a = new VarInt(64000)
    assertEquals(BigInt.apply(a.serialize()), BigInt.apply("-196358"))
    assertEquals(64000, VarInt.parse(a.serialize().iterator).value)
  }

  it should "test ffff" in {
    val a = new VarInt(0xFFFFL)
    assertEquals(BigInt.apply(a.serialize()), BigInt.apply("-131073"))
    assertEquals(0xFFFFL, VarInt.parse(a.serialize()iterator).value)
  }

  it should "test ints" in {
    val a = new VarInt(0xAABBCCDDL)
    assertEquals(BigInt.apply(a.serialize()), BigInt.apply("-4868752470"))
    assertEquals(0xAABBCCDDL, VarInt.parse(a.serialize().iterator).value)
  }

  it should "test ffffffff" in {
    val a = new VarInt(0xFFFFFFFFL)
    assertEquals(BigInt.apply(a.serialize()), BigInt.apply("-4294967297"))
    assertEquals(0xFFFFFFFFL, VarInt.parse(a.serialize().iterator).value)
  }

  it should "test long" in {
    val a = new VarInt(0xCAFEBABEDEADBEEFL)
    assertEquals(BigInt.apply(a.serialize()), BigInt.apply("-1171307680875479350"))
    assertEquals(0xCAFEBABEDEADBEEFL, VarInt.parse(a.serialize().iterator).value)
  }

  it should "515" in {
    assert(515L == VarInt.parse(CryptoUtil.hexToBytes("fd0302").iterator).value)
  }


  def assertEquals(x: Any, y: Any) = {
    assert(x == y)
  }


}
