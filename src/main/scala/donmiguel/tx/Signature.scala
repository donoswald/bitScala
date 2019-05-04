package donmiguel.tx

import org.bouncycastle.util.BigIntegers


case class Signature(r: BigInt, s: BigInt) {

  def der(): Array[Byte] = {

    var rarr = BigIntegers.asUnsignedByteArray(r.bigInteger)
    if (rarr(0) >= Signature.decision) {
      rarr = Array.concat(Signature.nullByte, rarr)
    }
    var rlen = Array.ofDim[Byte](1)
    rlen(0) = rarr.length.toByte
    rarr = Array.concat(Signature.marker, rlen, rarr)

    var sarr = BigIntegers.asUnsignedByteArray(s.bigInteger)
    if (sarr(0) >= Signature.decision) {
      sarr = Array.concat(Signature.nullByte, sarr)
    }
    var slen = Array.ofDim[Byte](1)
    slen(0) = sarr.length.toByte
    sarr = Array.concat(Signature.marker, slen, sarr)

    var arr = Array.concat(rarr, sarr)
    var alen = Array.ofDim[Byte](1)
    alen(0) = arr.length.toByte

    return Array.concat(Signature.start, alen, arr)
  }
}

object Signature {
  val nullByte = Array(0x00.toByte)
  val decision = 0x80.toByte
  val marker = Array(0x02.toByte)
  val start = Array(0x30.toByte)

  def parse(arr: Array[Byte]): Signature = {
    require(arr != null)
    var it = arr.iterator
    require(nextByte(it) == start(0))
    require(nextByte(it) == arr.length - 2)

    if (nextByte(it) != Signature.marker(0)) {
      throw new RuntimeException("expected marker byte")
    }
    var r = BigInt.apply(1, nextBytes(it, nextByte(it)))

    if (nextByte(it) != marker(0)) {
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
