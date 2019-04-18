package donmiguel

import java.math.BigInteger

import org.bouncycastle.crypto.digests.{GeneralDigest, RIPEMD160Digest, SHA256Digest}

import scala.annotation.tailrec

object CryptoUtil {
  val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val map = alphabet.zipWithIndex.toMap

  def sha256(bytes: Array[Byte]): Array[Byte] = {
    val digest = new SHA256Digest();
    hash(digest, bytes)
  }

  def hash160(bytes: Array[Byte]): Array[Byte] = {
    ripemd160(sha256(bytes))
  }

  def ripemd160(bytes: Array[Byte]): Array[Byte] = {
    val digest = new RIPEMD160Digest()
    hash(digest, bytes)
  }

  private def hash(digest: GeneralDigest, bytes: Array[Byte]): Array[Byte] = {
    digest.update(bytes, 0, bytes.length)
    val result = Array.ofDim[Byte](digest.getDigestSize)
    digest.doFinal(result, 0)
    result
  }

  def hexToBytes(hex: String): Array[Byte] = {
    hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def bytesToHex(bytes: Array[Byte], sep: Option[String] = None): String = {
    bytes.map("%02x".format(_)).mkString
  }

  def checksumBase58(input: Array[Byte]): String = {
    val doubleHash = sha256(sha256(input))
    val first4 = Array.ofDim[Byte](4)
    Array.copy(doubleHash, 0, first4, 0, 4)
    encodeBase58(Array.concat(input, first4))
  }

  def encodeBase58(input: Array[Byte]): String = {
    if (input.isEmpty) ""
    else {
      val big = new BigInteger(1, input)
      val builder = new StringBuilder

      @tailrec
      def encode1(current: BigInteger): Unit = current match {
        case BigInteger.ZERO => ()
        case _ =>
          val Array(x, remainder) = current.divideAndRemainder(BigInteger.valueOf(58L))
          builder.append(alphabet.charAt(remainder.intValue))
          encode1(x)
      }

      encode1(big)
      input.takeWhile(_ == 0).map(_ => builder.append(alphabet.charAt(0)))
      builder.toString().reverse
    }
  }

  def decodeBase58(input: String): Array[Byte] = {
    val zeroes = input.takeWhile(_ == '1').map(_ => 0: Byte).toArray
    val trim = input.dropWhile(_ == '1').toList
    val decoded = trim.foldLeft(BigInteger.ZERO)((a, b) => a.multiply(BigInteger.valueOf(58L)).add(BigInteger.valueOf(map(b))))
    if (trim.isEmpty) zeroes else zeroes ++ decoded.toByteArray.dropWhile(_ == 0) // BigInteger.toByteArray may add a leading 0x00
  }


  def fromVarint(buf: Array[Byte]): BigInt = {
    val first = 0xFF & buf(0)
    var value = 0L
    if (first < 253) value = first
    else if (first == 253) value = readUint16LE(buf, 1)
    else if (first == 254) value = readUint32LE(buf, 1)
    else value = readInt64LE(buf, 1)
    value
  }


  private def readInt64LE(bytes: Array[Byte], offset: Int): Long = {
    (bytes(offset) & 0xffl) |
      ((bytes(offset + 1) & 0xffl) << 8) |
      ((bytes(offset + 2) & 0xffl) << 16) |
      ((bytes(offset + 3) & 0xffl) << 24) |
      ((bytes(offset + 4) & 0xffl) << 32) |
      ((bytes(offset + 5) & 0xffl) << 40) |
      ((bytes(offset + 6) & 0xffl) << 48) |
      ((bytes(offset + 7) & 0xffl) << 56)
  }

  private def readUint16LE(bytes: Array[Byte], offset: Int): Int = {
    (bytes(offset) & 0xff) |
      ((bytes(offset + 1) & 0xff) << 8)
  }

 private def readUint32LE(bytes: Array[Byte], offset: Int): Long = {
    (bytes(offset) & 0xffl) |
      ((bytes(offset + 1) & 0xffl) << 8) |
      ((bytes(offset + 2) & 0xffl) << 16) |
      ((bytes(offset + 3) & 0xffl) << 24)
  }

  def toVarint(value:BigInt): Array[Byte] = {
    var bytes:Array[Byte] = null
    sizeOf(value.toLong) match {
      case 1 =>
        Array[Byte](value.toByte)
      case 3 =>
        bytes = new Array[Byte](3)
        bytes(0) = 253.toByte
        uint16ToByteArrayLE(value.toInt, bytes, 1)
        bytes
      case 5 =>
        bytes = new Array[Byte](5)
        bytes(0) = 254.toByte
        uint32ToByteArrayLE(value.toLong, bytes, 1)
        bytes
      case _ =>
        bytes = new Array[Byte](9)
        bytes(0) = 255.toByte
        int64ToByteArrayLE(value.toLong, bytes, 1)
        bytes
    }
  }

  private  def uint16ToByteArrayLE(value: Int, out: Array[Byte], offset: Int): Unit = {
    out(offset) = (0xFF & value).toByte
    out(offset + 1) = (0xFF & (value >> 8)).toByte
  }

  private def uint32ToByteArrayLE(value: Long, out: Array[Byte], offset: Int): Unit = {
    out(offset) = (0xFF & value).toByte
    out(offset + 1) = (0xFF & (value >> 8)).toByte
    out(offset + 2) = (0xFF & (value >> 16)).toByte
    out(offset + 3) = (0xFF & (value >> 24)).toByte
  }

  private def int64ToByteArrayLE(value: Long, out: Array[Byte], offset: Int): Unit = {
    out(offset) = (0xFF & value).toByte
    out(offset + 1) = (0xFF & (value >> 8)).toByte
    out(offset + 2) = (0xFF & (value >> 16)).toByte
    out(offset + 3) = (0xFF & (value >> 24)).toByte
    out(offset + 4) = (0xFF & (value >> 32)).toByte
    out(offset + 5) = (0xFF & (value >> 40)).toByte
    out(offset + 6) = (0xFF & (value >> 48)).toByte
    out(offset + 7) = (0xFF & (value >> 56)).toByte
  }


  private  def sizeOf(value: Long): Int = { // if negative, it's actually a very large unsigned long value
    if (value < 0) return 9 // 1 marker + 8 data bytes
    if (value < 253) return 1 // 1 data byte
    if (value <= 0xFFFFL) return 3 // 1 marker + 2 data bytes
    if (value <= 0xFFFFFFFFL) return 5 // 1 marker + 4 data bytes
    9
  }
}