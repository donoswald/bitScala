package donmiguel

trait Element {
  def == (that:Element):Boolean
  def != (that:Element):Boolean = {
    !(this==that)
  }
  def +(that:Element):Element
  def -(that:Element):Element
  def *(that:Element):Element
  def *(coefficient:Int):Element
  def **(exponent:Int):Element
  def /(that:Element):Element
}
