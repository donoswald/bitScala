package donmiguel.script

import donmiguel.UnitSpec

class OpCodeSpec extends UnitSpec {

  it should "encode/decode  zero" in {
    assert(OpCode.decode(OpCode.encode(0)) == 0)
  }

  it should "encode/decode negative zero" in {
    assert(OpCode.decode(OpCode.encode(0x80)) == 0x80)
  }

  it should "encode/decode Byte" in {
    for (i <- 0 to 256) {
      assert(OpCode.decode(OpCode.encode(i)) == i)
    }
  }

  it should "encode/decode negative Byte" in {
    for (i <- 0 to -256 by -1) {
      assert(OpCode.decode(OpCode.encode(i)) == i)
    }
  }

  it should "encode/decode long max/min" in {
    assert(OpCode.decode(OpCode.encode(Long.MaxValue)) == Long.MaxValue)
    assert(OpCode.decode(OpCode.encode(Long.MinValue + 1)) == Long.MinValue + 1)
  }

  it should "cast to bool" in {

    assert(OpCode.toBool(OpCode.encode(0L)) == false)

    assert(OpCode.toBool(OpCode.encode(0x00)) == false)

    assert(OpCode.toBool(OpCode.encode(0x80)) == false)

    assert(OpCode.toBool(OpCode.encode(0x01)) == true)


    assert(OpCode.toBool(OpCode.encode(0xff)) == true)

    assert(OpCode.toBool(OpCode.encode(0x0080)) == false)

  }

}
