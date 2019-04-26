package donmiguel.util

import donmiguel.UnitSpec

class LeConverterSpec extends UnitSpec {

  it should "read le" in {

    var v = CryptoUtil.hexToBytes("01000000")
    assert(LeConverter.readLongLE(v.iterator, 4) == 1)


  }

  it should "read le 2" in {
    var v = CryptoUtil.hexToBytes("99c3980000000000")

    assert(LeConverter.readLongLE(v.iterator, 8) == 10011545)

    v = CryptoUtil.hexToBytes("a135ef0100000000")
    assert(LeConverter.readLongLE(v.iterator, 8) == 32454049)

  }

  it should "read 32 bytes" in {
    var v = CryptoUtil.hexToBytes("813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1")

    assert(CryptoUtil.bytesToHex(LeConverter.readByteArrayLE(v.iterator, 32, 0)) == "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81")

  }

  it should "write le" in {

    var arr = new Array[Byte](4)
    LeConverter.writeLE(1,arr,0)

    assert(arr.deep == Array(1,0,0,0).deep)

    arr = new Array[Byte](8)
    LeConverter.writeLE(10011545,arr,0)

    assert(arr.deep==Array(-103,-61,-104,0,0,0,0,0).deep)
  }

}
