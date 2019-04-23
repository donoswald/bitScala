package donmiguel

object LeConverter {


  def readIntLE(it: Iterator[Byte], length: Int, offset: Int): Int = {
    var bytes = new Array[Byte](length)
    it.copyToArray(bytes, offset, length)
    BigInt.apply(bytes.reverse).toInt

  }

  def readByteArrayLE(it: Iterator[Byte], length: Int, offset: Int): Array[Byte] = {
    var bytes = new Array[Byte](length)
    it.copyToArray(bytes, 0, length)
    bytes.reverse

  }


  def readInt64LE(bytes: Array[Byte], offset: Int): Long = {
    (bytes(offset) & 0xffl) |
      ((bytes(offset + 1) & 0xffl) << 8) |
      ((bytes(offset + 2) & 0xffl) << 16) |
      ((bytes(offset + 3) & 0xffl) << 24) |
      ((bytes(offset + 4) & 0xffl) << 32) |
      ((bytes(offset + 5) & 0xffl) << 40) |
      ((bytes(offset + 6) & 0xffl) << 48) |
      ((bytes(offset + 7) & 0xffl) << 56)
  }

  def readUint16LE(bytes: Array[Byte], offset: Int): Long = {
    (bytes(offset) & 0xffl) |
      ((bytes(offset + 1) & 0xffl) << 8)
  }

  def readUint32LE(bytes: Array[Byte], offset: Int): Long = {
    (bytes(offset) & 0xffl) |
      ((bytes(offset + 1) & 0xffl) << 8) |
      ((bytes(offset + 2) & 0xffl) << 16) |
      ((bytes(offset + 3) & 0xffl) << 24)
  }


  def uint16ToByteArrayLE(value: Int, out: Array[Byte], offset: Int): Unit = {
    out(offset) = (0xFF & value).toByte
    out(offset + 1) = (0xFF & (value >> 8)).toByte
  }

  def uint32ToByteArrayLE(value: Long, out: Array[Byte], offset: Int): Unit = {
    out(offset) = (0xFF & value).toByte
    out(offset + 1) = (0xFF & (value >> 8)).toByte
    out(offset + 2) = (0xFF & (value >> 16)).toByte
    out(offset + 3) = (0xFF & (value >> 24)).toByte
  }

  def int64ToByteArrayLE(value: Long, out: Array[Byte], offset: Int): Unit = {
    out(offset) = (0xFF & value).toByte
    out(offset + 1) = (0xFF & (value >> 8)).toByte
    out(offset + 2) = (0xFF & (value >> 16)).toByte
    out(offset + 3) = (0xFF & (value >> 24)).toByte
    out(offset + 4) = (0xFF & (value >> 32)).toByte
    out(offset + 5) = (0xFF & (value >> 40)).toByte
    out(offset + 6) = (0xFF & (value >> 48)).toByte
    out(offset + 7) = (0xFF & (value >> 56)).toByte
  }

}
