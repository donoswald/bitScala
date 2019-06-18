package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import com.sun.jna.platform.win32.WinDef.ULONG
import donmiguel.script.Script
import donmiguel.util.LeConverter

case class TxOut(amount: ULONG, scriptPubkey: Script) {
  def serialize: Array[Byte] = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putLong(this.amount.longValue())
      .order(ByteOrder.BIG_ENDIAN)
      .put(this.scriptPubkey.serialize)
    bb.array().slice(0, bb.position())
  }
}

object TxOut {

  def parse(it: Iterator[Byte]): TxOut = {

    var amount = new ULONG(LeConverter.readLongLE(it, 8))
    var script = Script.parse(it)

    new TxOut(amount, script)
  }
}
