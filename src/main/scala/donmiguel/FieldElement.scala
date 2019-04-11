package donmiguel

class FieldElement(_num: BigInt, _prime: BigInt) {

  require(_num >= 0, "num is negativ " + _num)
  require(_num < _prime, "num is < prime" + _num)
  require(_prime > 0, "prime is <= 0" + _prime)

  val num: BigInt = _num
  val prime: BigInt = _prime

  def ==(that: FieldElement) = this._prime == that.prime && this._num == that.num

  def !=(that: FieldElement) = !(this == that)

  def +(that: FieldElement): FieldElement = {
    require(this.prime == that.prime, "primes don't match")
    new FieldElement((this.num + that.num) % prime, prime)
  }

  def -(that: FieldElement): FieldElement = {
    require(this.prime == that.prime, "primes don't match")
    var num = this.num - that.num % prime
    if (num < 0) {
      num += prime
    }
    new FieldElement(num, prime)
  }

  def *(that: FieldElement): FieldElement = {
    require(this.prime == that.prime, "primes don't match")
    new FieldElement((this.num * that.num) % prime, prime)
  }

  def *(cofficient: Int): FieldElement = {
    new FieldElement((this.num * cofficient) % this.prime, this.prime)
  }

  def **(exponent: Int): FieldElement = {
    var n = exponent % (this.prime - 1)
    new FieldElement(this.num.modPow(n, this.prime), this.prime)
  }

  def /(that: FieldElement): FieldElement = {
    require(this.prime == that.prime, "primes don't match")

    val num = this.num * that.num.modPow(prime - 2, prime) % prime
    new FieldElement(num, prime)
  }

  override def toString: String = StringBuilder.newBuilder+ "prime: " + prime + " ,num: " + num
}
