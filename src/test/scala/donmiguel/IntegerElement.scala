package donmiguel

class IntegerElement(var _x: Int) extends Element {
  require(_x != null)
  val x = _x

  override def +(that: Element): Element = {
    new IntegerElement(this.x + cast(that).x)
  }

  override def -(that: Element): Element = {
    new IntegerElement(this.x - cast(that).x)
  }

  override def *(that: Element): Element = {
    new IntegerElement(this.x * cast(that).x)
  }

  override def *(coefficient: Int): Element = {
    new IntegerElement(coefficient * this.x)
  }

  override def **(exponent: Int): Element = {
    new IntegerElement(Math.pow(this.x, exponent).toInt)
  }

  override def /(that: Element): Element = {
      new IntegerElement(this.x / cast(that).x)
  }

  override def ==(that: Element) = {
    this.x == cast(that).x
  }

  override def toString: String = s"x: $x"

  private def cast(element: Element): IntegerElement = {
    require(element.isInstanceOf[IntegerElement])
    return element.asInstanceOf[IntegerElement]
  }
}
