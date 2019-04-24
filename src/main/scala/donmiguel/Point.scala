package donmiguel

class Point(val x: Element, val y: Element, val a: Element, val b: Element) {
  require(x != null)
  require(y != null)

  if (x != ElementNone && y != ElementNone) {
    assert(y ** 2 == x ** 3 + a * x + b, "the point is not on the curve")
  } else {
    require(x == ElementNone && y == ElementNone, "either x and y must be None")
  }


  def !=(that: Point): Boolean = {
    !(this == that)
  }

  def ==(that: Point): Boolean = {

    this.x == that.x &&
      this.y == that.y &&
      this.a == that.a &&
      this.b == that.b
  }

  def *(coefficient: BigInt): Point = {
    var coef = coefficient
    var cur = this
    var result = new Point(ElementNone, ElementNone, this.a, this.b)
    while (coef != 0) {
      if ((coef & 1) == 1) {
        result += cur
      }
      cur += cur
      coef >>= 1
    }
    result
  }

  def +(that: Point): Point = {
    require(this.a == that.a && this.b == that.b, "the points are not on the same curve")

    //Case 0.0: self is the point at infinity, return other
    if (this.x == ElementNone && y == ElementNone) {
      return that
    }
    //Case 0.1: other is the point at infinity, return self
    if (that.x == ElementNone && that.y == ElementNone) {
      return this
    }

    //Case 1: self.x == other.x, self.y != other.y
    //Result is point at infinity
    if (this.x == that.x && this.y != that.y) {
      return new Point(ElementNone, ElementNone, this.a, this.b)
    }
    //Case 2: self.x != other.x
    //Formula (x3,y3)==(x1,y1)+(x2,y2)
    //s=(y2-y1)/(x2-x1)
    //x3=s**2-x1-x2
    //y3=s*(x1-x3)-y1
    if (this.x != that.x) {

      var s = (that.y - this.y) / (that.x - this.x)
      var x = s ** 2 - this.x - that.x
      var y = s * (this.x - x) - this.y

      Option.apply(x)
      return new Point(x, y, this.a, this.b)
    }

    //Case 4: if we are tangent to the vertical line,
    //we return the point at infinity
    //note instead of figuring out what 0 is for each type
    //we just use 0 * self.x

    if (this == that && this.y == this.x * 0) {
      return new Point(ElementNone, ElementNone, this.a, this.b)
    }

    //Case 3: self == other
    //Formula (x3,y3)=(x1,y1)+(x1,y1)
    //s=(3*x1**2+a)/(2*y1)
    //x3=s**2-2*x1
    //y3=s*(x1-x3)-y1
    if (this == that) {
      var s = ((this.x ** 2) * 3 + this.a) / (this.y * 2)
      var x = s ** 2 - this.x * 2
      var y = s * (this.x - x) - this.y
      return new Point(x, y, this.a, this.b)
    }
    throw new IllegalStateException("should not reach here")
  }

  override def toString: String = StringBuilder.newBuilder + "x: " + x + "\ny: " + y + "\na: " + a + "\nb: " + b


}
