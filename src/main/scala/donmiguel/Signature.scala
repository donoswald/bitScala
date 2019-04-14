package donmiguel

class Signature(_r: BigInt, _s: BigInt) {
  val r = _r
  val s = _s

  override def toString:String=  s"s = $s\nr = $r"
}
