package donmiguel.util

object LeConverter {


  def readLongLE(it: Iterator[Byte], length: Int): Long = {

    var result = 0L
    for (i <- 0 until length) {
      if (it.hasNext)
        result |= (it.next() & 0xffl) << i * 8
    }
    result

  }

  def readLongLE(bytes: Array[Byte], offset: Int): Long = {
    var result = 0L
    for (i <- offset until bytes.length) {
      result |= (bytes(i) & 0xffl) << (i - offset) * 8
    }
    result
  }

  def readByteArrayLE(it: Iterator[Byte], length: Int, offset: Int): Array[Byte] = {
    var bytes = new Array[Byte](length)
    it.copyToArray(bytes, 0, length)
    bytes.reverse

  }

  def writeLE(value: Long, out: Array[Byte], offset: Int): Unit = {
    for (i <- offset until out.length) {
      out(i) = (0xff & (value >> (i - offset) * 8)).toByte
    }
  }

  def writeLE(value: Long, length: Int): Array[Byte] = {
    var result = new Array[Byte](length)
    for (i <- 0 until length) {
      result(i) = (0xff & (value >> i * 8)).toByte
    }
    result
  }
}
