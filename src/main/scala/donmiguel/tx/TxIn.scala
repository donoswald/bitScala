package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import com.sun.jna.platform.win32.WinDef.UINT
import donmiguel.script.Script
import donmiguel.util.{CryptoUtil, LeConverter}

case class TxIn(prevTx: String, prevIdx: UINT, var scriptSig: Script = new Script(), sequence:UINT = new UINT(0xffffffff)  ) {

  def value: Long = {
    var tx = TxFetcher.cache(prevTx)
    tx.outs(prevIdx.intValue()).amount.longValue()
  }

  def scriptPubkey: Script = {
    var tx = TxFetcher.cache(prevTx)
    tx.outs(prevIdx.intValue()).scriptPubkey
  }

  def serialize: Array[Byte] = {
    var bb = ByteBuffer.allocate(999999999)
      .put(CryptoUtil.hexToBytes(this.prevTx).reverse)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(prevIdx.intValue())
      .order(ByteOrder.BIG_ENDIAN)
      .put(this.scriptSig.serialize)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(this.sequence.intValue())
      .order(ByteOrder.BIG_ENDIAN)

    return bb.array().slice(0, bb.position())
  }
}

object TxIn {

  def parse(it: Iterator[Byte]): TxIn = {

    val txPrev = CryptoUtil.bytesToHex(LeConverter.readByteArrayLE(it, 32, 0))
    val prevIdx = new UINT(LeConverter.readLongLE(it, 4))

    val script = Script.parse(it)

    val sequence = new UINT(LeConverter.readLongLE(it, 4))

    new TxIn(txPrev, prevIdx, script, sequence)

  }
}

