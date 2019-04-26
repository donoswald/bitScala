package donmiguel.crypto

trait Element {

  def +(that: Element): Element

  def -(that: Element): Element

  def *(that: Element): Element

  def *(coefficient: BigInt): Element

  def **(exponent: BigInt): Element

  def /(that: Element): Element
}

object ElementNone extends Element {

  override def +(that: Element): Element = throw new RuntimeException("not implemented")

  override def -(that: Element): Element = throw new RuntimeException("not implemented")

  override def *(that: Element): Element = throw new RuntimeException("not implemented")

  override def *(coefficient: BigInt): Element = throw new RuntimeException("not implemented")

  override def **(exponent: BigInt): Element = throw new RuntimeException("not implemented")

  override def /(that: Element): Element = throw new RuntimeException("not implemented")

  def ==(that: Element): Boolean = null != that
}
