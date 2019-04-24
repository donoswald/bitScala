package donmiguel

case class FiniteFieldElement(val num: BigInt, val prime: BigInt) extends Element {
  require(num >= 0, "num is negativ " + num)
  require(num < prime, "num is < prime" + num)
  require(prime > 0, "prime is <= 0" + prime)


  override def +(that: Element): Element = {
    var other = cast(that)
    require(this.prime == other.prime, "primes don't match")
    new FiniteFieldElement((this.num + other.num) % prime, prime)
  }

  override def -(that: Element): Element = {
    var other = cast(that)
    require(this.prime == other.prime, "primes don't match")
    var num = this.num - other.num % prime
    if (num < 0) {
      num += prime
    }
    new FiniteFieldElement(num, prime)
  }

  private def cast(element: Element): FiniteFieldElement = {
    require(element.isInstanceOf[FiniteFieldElement])
    return element.asInstanceOf[FiniteFieldElement]
  }

  override def *(that: Element): Element = {
    var other = cast(that)
    require(this.prime == other.prime, "primes don't match")
    new FiniteFieldElement((this.num * other.num) % prime, prime)
  }

  override def *(cofficient: BigInt): Element = {
    new FiniteFieldElement((this.num * cofficient) % this.prime, this.prime)
  }

  override def **(exponent: BigInt): Element = {
    var n = exponent % (this.prime - 1)
    new FiniteFieldElement(this.num.modPow(n, this.prime), this.prime)
  }

  override def /(that: Element): Element = {
    var other = cast(that)
    require(this.prime == other.prime, "primes don't match")

    val num = this.num * other.num.modPow(prime - 2, prime) % prime
    new FiniteFieldElement(num, prime)
  }

  override def toString: String = s"prime: $prime, num: $num"
}
