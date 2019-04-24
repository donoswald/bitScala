package donmiguel

object VarInt {

  def fromVarint(it: Iterator[Byte]): Long = {
    val first = 0xFF & it.next()
    var value = 0L

    if (first < 253) {
      value = first
    } else if (first == 253) {
      var buf = new Array[Byte](3)
      buf(0)=first.asInstanceOf[Byte]
      it.copyToArray(buf,1,2)
      value = LeConverter.readLongLE(buf, 1)
    }
    else if (first == 254) {
      var buf = new Array[Byte](5)
      buf(0)=first.asInstanceOf[Byte]
      it.copyToArray(buf,1,4)
      value = LeConverter.readLongLE(buf, 1)
    }
    else{
      var buf = new Array[Byte](9)
      buf(0)=first.asInstanceOf[Byte]
      it.copyToArray(buf,1,8)
      value = LeConverter.readLongLE(buf, 1)
    }
    value
  }

  def toVarint(value:Long): Array[Byte] = {
    var bytes:Array[Byte] = null
    sizeOf(value.toLong) match {
      case 1 =>
        Array[Byte](value.toByte)
      case 3 =>
        bytes = new Array[Byte](3)
        bytes(0) = 253.toByte
        LeConverter. writeLE(value.toLong, bytes, 1)
        bytes
      case 5 =>
        bytes = new Array[Byte](5)
        bytes(0) = 254.toByte
        LeConverter.writeLE(value.toLong, bytes, 1)
        bytes
      case _ =>
        bytes = new Array[Byte](9)
        bytes(0) = 255.toByte
        LeConverter.writeLE(value.toLong, bytes, 1)
        bytes
    }
  }

  private  def sizeOf(value: Long): Int = { // if negative, it's actually a very large unsigned long value
    if (value < 0) return 9 // 1 marker + 8 data bytes
    if (value < 253) return 1 // 1 data byte
    if (value <= 0xFFFFL) return 3 // 1 marker + 2 data bytes
    if (value <= 0xFFFFFFFFL) return 5 // 1 marker + 4 data bytes
    9
  }

}
