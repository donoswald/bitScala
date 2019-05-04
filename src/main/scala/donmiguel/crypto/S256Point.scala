package donmiguel.crypto

import donmiguel.tx.Signature
import donmiguel.util.CryptoUtil
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.crypto.signers.ECDSASigner
import org.bouncycastle.util.BigIntegers

class S256Point(x: Element, y: Element) extends Point(x, y, new S256Element(Secp256k1.A), new S256Element(Secp256k1.B)) {

  override def +(that: Point): S256Point = new S256Point((super.+(that)))

  override def *(coefficient: BigInt): S256Point = new S256Point(super.*(coefficient))

  def this(point: Point) {
    this(point.x, point.y)
  }

    def verify(message: String, sig: Signature): Boolean = {

    val bytes = message.getBytes("UTF-8")
    val hash = Secp256k1.doubleDigest(bytes)
    verify(hash, sig)
  }

  def verify(hash: Array[Byte], sig: Signature): Boolean = {
    val signer = new ECDSASigner
    val params = new ECPublicKeyParameters(Secp256k1.ecParams.getCurve.decodePoint(this.sec(false)), Secp256k1.ecParams)
    signer.init(false, params)
    signer.verifySignature(hash, sig.r.bigInteger, sig.s.bigInteger)
  }

  def address(compressed: Boolean = true, testnet: Boolean = true): String = {

    val hash = CryptoUtil.hash160(sec(compressed))
    var prefix: Byte = 0x00
    if (testnet) {
      prefix = 0x6f.toByte
    }
    return CryptoUtil.checksumBase58(Array.concat(Array(prefix), hash))
  }

  def sec(compressed: Boolean): Array[Byte] = {
    require(this.x != ElementNone && this.y != ElementNone)
    require(this.x.isInstanceOf[FiniteFieldElement])
    require(this.y.isInstanceOf[FiniteFieldElement])

    var x = this.x.asInstanceOf[FiniteFieldElement].num
    var y = this.y.asInstanceOf[FiniteFieldElement].num
    var xarr = BigIntegers.asUnsignedByteArray(x.bigInteger)
    var yarr = BigIntegers.asUnsignedByteArray(y.bigInteger)


    var first = Array.ofDim[Byte](1)
    if (compressed) {
      if (y % 2 == 0) {
        first(0) = 0x02
        return Array.concat(first, xarr)
      } else {
        first(0) = 0x03
        return Array.concat(first, xarr)
      }
    } else {
      first(0) = 0x04
      return Array.concat(first, xarr, yarr)
    }

  }
}

object S256Point {

  def parse(arr: Array[Byte]): S256Point = {
    require(arr != null)
    require(arr.length > 32)

    var xarr = Array.ofDim[Byte](32)
    Array.copy(arr, 1, xarr, 0, 32)

    arr(0) match {
      case 0x04 => {
        require(arr.length == 65)

        var x = BigInt.apply(1, xarr)

        var yarr = Array.ofDim[Byte](32)
        Array.copy(arr, 33, yarr, 0, 32)
        var y = BigInt.apply(1, yarr)

        return new S256Point(new S256Element(x), new S256Element(y))
      }
      case _ => {
        require(arr.length == 33)
        var even = arr(0) == 0x02

        var x = new S256Element(BigInt.apply(1, xarr))
        var alpha = (x ** 3 + new S256Element(Secp256k1.B)).asInstanceOf[S256Element]
        var beta = alpha.sqrt()

        var even_beta: S256Element = null
        var odd_beta: S256Element = null

        if (beta.num % 2 == 0) {
          even_beta = beta
          odd_beta = new S256Element(Secp256k1.P - beta.num)
        } else {
          even_beta = new S256Element(Secp256k1.P - beta.num)
          odd_beta = beta
        }

        if (even) {
          return new S256Point(x, even_beta)
        } else {
          return new S256Point(x, odd_beta)
        }
      }
    }
  }
}
