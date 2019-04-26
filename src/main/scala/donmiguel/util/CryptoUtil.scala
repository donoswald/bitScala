package donmiguel.util

import java.math.BigInteger

import org.bouncycastle.crypto.digests.{GeneralDigest, RIPEMD160Digest, SHA256Digest}

import scala.annotation.tailrec

object CryptoUtil {
  val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val map = alphabet.zipWithIndex.toMap

  def hash160(bytes: Array[Byte]): Array[Byte] = {
    ripemd160(sha256(bytes))
  }

  def ripemd160(bytes: Array[Byte]): Array[Byte] = {
    val digest = new RIPEMD160Digest()
    hash(digest, bytes)
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

  def sha256(bytes: Array[Byte]): Array[Byte] = {
    val digest = new SHA256Digest();
    hash(digest, bytes)
  }

  private def hash(digest: GeneralDigest, bytes: Array[Byte]): Array[Byte] = {
    digest.update(bytes, 0, bytes.length)
    val result = Array.ofDim[Byte](digest.getDigestSize)
    digest.doFinal(result, 0)
    result
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

}
