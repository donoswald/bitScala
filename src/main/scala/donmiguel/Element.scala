package donmiguel

trait Element {
  protected[this] type T

  def +(that:T):T
  def -(that:T):T
  def *(that:T):T
  def *(coefficient:Int):T
  def **(exponent:Int):T
  def /(that:T):T
}
