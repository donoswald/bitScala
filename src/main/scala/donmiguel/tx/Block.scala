package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import com.sun.jna.platform.win32.WinDef.UINT
import donmiguel.util.{CryptoUtil, LeConverter}

class Block(val version: UINT, val prevBlock: String, val merkleRoot: String, val timestamp: UINT, val bits: Array[Byte], val nonce: Array[Byte]) {

  def serialize(): Array[Byte] = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(version.intValue())
      .put(CryptoUtil.hexToBytes(prevBlock).reverse)
      .put(CryptoUtil.hexToBytes(merkleRoot).reverse)
      .putInt(timestamp.intValue())
      .order(ByteOrder.BIG_ENDIAN)
      .put(bits)
      .put(nonce)
    return bb.array().slice(0, bb.position())
  }

  def hash(): Array[Byte] = {
    CryptoUtil.doubleSha256(serialize()).reverse
  }

  def bip9(): Boolean = {
    (this.version.intValue() >> 29) == 0x001
  }

  def bip91(): Boolean = {
    (this.version.intValue() >> 4 & 1) == 1
  }

  def bip141(): Boolean = {
    (this.version.intValue() >> 1 & 1) == 1
  }
}

object Block {
  def parse(it: Iterator[Byte]): Block = {

    val version = LeConverter.readLongLE(it, 4)
    val prevBlock = CryptoUtil.bytesToHex(LeConverter.readByteArrayLE(it, 32, 0))
    val merkleRoot = CryptoUtil.bytesToHex(LeConverter.readByteArrayLE(it, 32, 0))
    val timestamp = LeConverter.readLongLE(it, 4)

    var bits = new Array[Byte](4)
    it.copyToArray(bits, 0, 4)

    var nonce = new Array[Byte](4)
    it.copyToArray(nonce, 0, 4)

    return new Block(new UINT(version), prevBlock, merkleRoot, new UINT(timestamp), bits, nonce)
  }
}
