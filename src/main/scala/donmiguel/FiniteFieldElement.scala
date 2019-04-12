package donmiguel

class FiniteFieldElement(_num: BigInt, _prime: BigInt)  extends Element {
  type T = FiniteFieldElement
  require(_num >= 0, "num is negativ " + _num)
  require(_num < _prime, "num is < prime" + _num)
  require(_prime > 0, "prime is <= 0" + _prime)

  val num: BigInt = _num
  val prime: BigInt = _prime

  def ==(that: FiniteFieldElement) = this._prime == that.prime && this._num == that.num

  def !=(that: FiniteFieldElement) = !(this == that)

  def +(that: FiniteFieldElement): FiniteFieldElement = {
    require(this.prime == that.prime, "primes don't match")
    new FiniteFieldElement((this.num + that.num) % prime, prime)
  }

  def -(that: FiniteFieldElement): FiniteFieldElement = {
    require(this.prime == that.prime, "primes don't match")
    var num = this.num - that.num % prime
    if (num < 0) {
      num += prime
    }
    new FiniteFieldElement(num, prime)
  }

  def *(that: FiniteFieldElement): FiniteFieldElement = {
    require(this.prime == that.prime, "primes don't match")
    new FiniteFieldElement((this.num * that.num) % prime, prime)
  }

  def *(cofficient: Int): FiniteFieldElement = {
    new FiniteFieldElement((this.num * cofficient) % this.prime, this.prime)
  }

  def **(exponent: Int): FiniteFieldElement = {
    var n = exponent % (this.prime - 1)
    new FiniteFieldElement(this.num.modPow(n, this.prime), this.prime)
  }

  def /(that: FiniteFieldElement): FiniteFieldElement = {
    require(this.prime == that.prime, "primes don't match")

    val num = this.num * that.num.modPow(prime - 2, prime) % prime
    new FiniteFieldElement(num, prime)
  }


  override def toString: String = s"prime: $prime, num: $num"
}
