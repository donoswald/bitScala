package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import donmiguel.script.Script
import donmiguel.util.{CryptoUtil, LeConverter}

case class TxIn(prev_tx: String, prev_idx: Int,var script_sig: Script = new Script(), sequence: Long = 0xffffffff) {

  def value: Long = {
    var tx = TxFetcher.cache(prev_tx)
    tx.outs(prev_idx).amount
  }

  def script_pubkey: Script = {
    var tx = TxFetcher.cache(prev_tx)
    tx.outs(prev_idx).script_pubkey
  }

  def serialize: Array[Byte] = {
    var bb = ByteBuffer.allocate(999999999)
      .put(CryptoUtil.hexToBytes(this.prev_tx).reverse)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(prev_idx)
      .order(ByteOrder.BIG_ENDIAN)
      .put(this.script_sig.serialize)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(this.sequence.toInt)
      .order(ByteOrder.BIG_ENDIAN)

    println(  CryptoUtil.bytesToHex(  bb.duplicate().array().slice(0,bb.position())))


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

