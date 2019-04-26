package donmiguel.crypto

class S256Element(num: BigInt) extends FiniteFieldElement(num, Secp256k1.P) {

  def sqrt(): S256Element = {
    return (this ** ((Secp256k1.P + 1) / 4))
  }

  override def **(exponent: BigInt): S256Element = {
    var field = super.**(exponent).asInstanceOf[FiniteFieldElement]
    return new S256Element(field.num)
  }

  override def +(that: Element): Element = {
    var field = super.+(that).asInstanceOf[FiniteFieldElement]
    return new S256Element(field.num)
  }

}
