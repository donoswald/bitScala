package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import donmiguel.script.Script
import donmiguel.util.{CryptoUtil, LeConverter}

case class TxIn(prevTx: String, prevIdx: Int, var scriptSig: Script = new Script(), sequence: Long = 0xffffffff) {

  def value: Long = {
    var tx = TxFetcher.cache(prevTx)
    tx.outs(prevIdx).amount
  }

  def script_pubkey: Script = {
    var tx = TxFetcher.cache(prevTx)
    tx.outs(prevIdx).scriptPubkey
  }

  def serialize: Array[Byte] = {
    var bb = ByteBuffer.allocate(999999999)
      .put(CryptoUtil.hexToBytes(this.prevTx).reverse)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(prevIdx)
      .order(ByteOrder.BIG_ENDIAN)
      .put(this.scriptSig.serialize)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(this.sequence.toInt)
      .order(ByteOrder.BIG_ENDIAN)

    return bb.array().slice(0, bb.position())
  }
}

object TxIn {

  def parse(it: Iterator[Byte]): TxIn = {

    val tx_prev = CryptoUtil.bytesToHex(LeConverter.readByteArrayLE(it, 32, 0))
    val prev_idx = LeConverter.readLongLE(it, 4).toInt

    val script = Script.parse(it)

    val sequence = LeConverter.readLongLE(it, 4)
    new TxIn(tx_prev, prev_idx, script, sequence)

  }
}

