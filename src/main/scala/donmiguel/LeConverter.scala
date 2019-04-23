package donmiguel

object LeConverter {


  def readLongLE(it: Iterator[Byte], length: Int, offset: Int): Long = {
    var bytes = new Array[Byte](length)
    it.copyToArray(bytes, offset, length)
    BigInt.apply(bytes.reverse).toInt

  }
  def readLongLE(bytes: Array[Byte], offset: Int): Long = {
    var result = 0L
    for (i <- offset to bytes.length-1 ) {
      result |= (bytes(i) & 0xffl) << (i - offset) * 8
    }
    result
  }

  def readByteArrayLE(it: Iterator[Byte], length: Int, offset: Int): Array[Byte] = {
    var bytes = new Array[Byte](length)
    it.copyToArray(bytes, 0, length)
    bytes.reverse

  }

  def writLong(value:Long, out:Array[Byte],offset:Int):Unit = {
    for(i<- offset to out.length-1){
      out(i)= (0xff & (value >> (i-offset)*8)).toByte
    }
  }
}
