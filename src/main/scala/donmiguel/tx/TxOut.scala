package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import donmiguel.script.Script
import donmiguel.util.LeConverter

case class TxOut(amount: Long, script_pubkey: Script) {
  def serialize: Array[Byte] = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putLong(this.amount.longValue())
      .order(ByteOrder.BIG_ENDIAN)
      .put(this.script_pubkey.serialize)
    bb.array().slice(0, bb.position())
  }
}

object TxOut {

  def parse(it: Iterator[Byte]): TxOut = {

    var amount = LeConverter.readLongLE(it, 8)
    var script = Script.parse(it)

    new TxOut(amount, script)
  }
}
