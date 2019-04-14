package donmiguel

import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.crypto.signers.ECDSASigner

class S256Point(_x: Element, _y: Element) extends Point(Some(_x), Some(_y), new S256Element(Secp256k1.A), new S256Element(Secp256k1.B)) {

  override def +(that: Point): S256Point = new S256Point((super.+(that)))

  override def *(coefficient: BigInt): S256Point = new S256Point(super.*(coefficient))

  def this(point:Point)  {
    this(point.x.get,point.y.get)
  }

  def sec(compressed: Boolean): Array[Byte] = {
    require(this.x != None && this.y != None)
    require(this.x.get.isInstanceOf[FiniteFieldElement])
    require(this.y.get.isInstanceOf[FiniteFieldElement])

    var x = this.x.get.asInstanceOf[FiniteFieldElement].num
    var y = this.y.get.asInstanceOf[FiniteFieldElement].num


    if (compressed) {
      if (y % 2 == 0) {
        var bytes=  x.toByteArray
        bytes.update(0,0x02)
        return bytes
      } else {
      var bytes=  x.toByteArray
        bytes.update(0,0x03)
        return bytes
      }
    } else {
      var bytes=  x.toByteArray
      bytes.update(0,0x04)
      return  Array.concat(bytes, y.toByteArray)
    }

  }

  def verify(message: String, sig: Signature): Boolean = {

    val bytes = message.getBytes("UTF-8")
    val hash = Secp256k1.doubleDigest(bytes)
    verify(hash, sig)
  }

  def verify(hash: Array[Byte], sig: Signature): Boolean = {
    val signer = new ECDSASigner
    val params = new ECPublicKeyParameters(Secp256k1.ecParams.getCurve().decodePoint(this.sec(false)), Secp256k1.ecParams)
    signer.init(false, params)
    signer.verifySignature(hash, sig.r.bigInteger, sig.s.bigInteger)

  }
}
