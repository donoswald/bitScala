package donmiguel

import org.bouncycastle.util.BigIntegers


case class Signature(r: BigInt, s: BigInt) {

  override def toString: String = s"s = $s\nr = $r"

  def der(): Array[Byte] = {
    val nullByte = Array.ofDim[Byte](1)
    nullByte(0) = 0x00
    val marker = Array.ofDim[Byte](1)
    marker(0) = 0x02
    val start = Array.ofDim[Byte](1)
    start(0) = 0x30

    var rarr = BigIntegers.asUnsignedByteArray(r.bigInteger)
    if (rarr(0) >= 0x80) {
      rarr = Array.concat(nullByte, rarr)
    }
    var rlen = Array.ofDim[Byte](1)
    rlen(0) = rarr.length.toByte
    rarr = Array.concat(marker, rlen, rarr)

    var sarr = BigIntegers.asUnsignedByteArray(s.bigInteger)
    if (sarr(0) >= 0x80) {
      sarr = Array.concat(nullByte, sarr)
    }
    var slen = Array.ofDim[Byte](1)
    slen(0) = sarr.length.toByte
    sarr = Array.concat(marker, slen, sarr)

    var arr = Array.concat(rarr, sarr)
    var alen = Array.ofDim[Byte](1)
    alen(0) = arr.length.toByte

    return Array.concat(start, alen, arr)
  }
}

object Signature {
  def parse(arr: Array[Byte]): Signature = {
    require(arr != null)
    var it = arr.iterator
    require(nextByte(it) == 0x30)
    require(nextByte(it) == arr.length - 2)

    if (nextByte(it) != 0x02) {
      throw new RuntimeException("expected marker byte")
    }
    var r = BigInt.apply(1, nextBytes(it, nextByte(it)))

    if (nextByte(it) != 0x02) {
      throw new RuntimeException("expected marker byte")
    }
    var s = BigInt.apply(1, nextBytes(it, nextByte(it)))

    return new Signature(r, s)

  }

  def nextByte(it: Iterator[Byte]): Byte = {
    return it.next()
  }

  def nextBytes(it: Iterator[Byte], n: Int): Array[Byte] = {
    var arr = Array.ofDim[Byte](n)
    (1 to n) foreach (i => arr(i - 1) = it.next())
    return arr
  }
}
