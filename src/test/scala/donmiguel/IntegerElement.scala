package donmiguel

case class IntegerElement(var x: BigInt) extends Element {

  override def +(that: Element): Element = {
    new IntegerElement(this.x + cast(that).x)
  }

  override def -(that: Element): Element = {
    new IntegerElement(this.x - cast(that).x)
  }

  override def *(that: Element): Element = {
    new IntegerElement(this.x * cast(that).x)
  }

  override def *(coefficient: BigInt): Element = {
    new IntegerElement(coefficient * this.x)
  }

  override def **(exponent: BigInt): Element = {
    //Fixme must convert to Int for exponentiation!
    new IntegerElement(this.x.pow(exponent.toInt))
  }

  override def /(that: Element): Element = {
    new IntegerElement(this.x / cast(that).x)
  }

  private def cast(element: Element): IntegerElement = {
    require(element.isInstanceOf[IntegerElement])
    return element.asInstanceOf[IntegerElement]
  }

  def ==(that: Element): Boolean = {
    if (null == that) {
      return false
    } else if (that == ElementNone) {
      return false
    }
    this.x == cast(that).x
  }

  override def toString: String = s"x: $x"
}
