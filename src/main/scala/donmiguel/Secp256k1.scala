package donmiguel

import java.security.MessageDigest

import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.params.ECDomainParameters
import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}
import org.bouncycastle.math.ec.custom.sec.SecP256K1Point

final object Secp256k1 {
  val A = 0
  val B = 7
  val P = BigInt.apply(2).pow(256) - BigInt.apply(2).pow(32) - 977
  val N = BigInt.apply("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)
  val G = new S256Point(
    new S256Element(BigInt.apply("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798", 16)),
    new S256Element(BigInt.apply("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8", 16))
  )

  val params: X9ECParameters = CustomNamedCurves.getByName("secp256k1")
  val ecParams = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)
  val digest = MessageDigest.getInstance("SHA-256")

  def doubleDigest(input: Array[Byte]): Array[Byte] = doubleDigest(input, 0, input.length)

  def doubleDigest(input: Array[Byte], offset: Int, length: Int): Array[Byte] = {
    var bytes: Array[Byte] = null

    this.synchronized {
      digest.reset()
      digest.update(input, offset, length)
      var first = digest.digest()
      bytes = digest.digest(first)
    }

    bytes
  }

  override def toString: String = s"A = $A\nB = $B\nP = $P\nN = $N\G = $G"
}
