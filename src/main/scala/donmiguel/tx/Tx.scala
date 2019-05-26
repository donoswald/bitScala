package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import donmiguel.script.{Script, ScriptElement}
import donmiguel.util.{CryptoUtil, LeConverter, VarInt}

case class Tx(version: Int, num_inputs: Long, ins: Array[TxIn], num_outs: Long, outs: Array[TxOut], locktime: Int, testnet: Boolean = false) {
  def fee: Long = {

    var sum_in: Long = 0
    var sum_out: Long = 0

    for (in <- ins) {
      sum_in += in.value
    }

    for (out <- outs) {
      sum_out += out.amount
    }

    return sum_in - sum_out
  }

  def serialize: Array[Byte] = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(this.version)
      .order(ByteOrder.BIG_ENDIAN)
      .put(VarInt.toVarint(this.ins.length))
    for (txIn <- ins) {
      bb.put(txIn.serialize)
    }

    bb.put(VarInt.toVarint(this.outs.length))
    for (txOut <- outs) {
      bb.put(txOut.serialize)
    }
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(this.locktime)
    bb.array().slice(0, bb.position())
  }

  def sigHash(input_index: Int, redeem_script: Option[Script] = Option.empty): Array[Byte] = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(version)
      .order(ByteOrder.BIG_ENDIAN)
      .put(VarInt.toVarint(this.ins.length))

    for (i <- 0 until this.ins.length) {
      val tx_in = this.ins(i)

      var script: Option[Script] = None

      if (i == input_index) {
        script = redeem_script.orElse(Some(tx_in.script_pubkey))
      }

      bb.put(new TxIn(
        tx_in.prevTx,
        tx_in.prevIdx,
        script.orNull,
        tx_in.sequence
      ).serialize)

    }
    bb.put(VarInt.toVarint(this.outs.length))
    for (txOut <- outs) {
      bb.put(txOut.serialize)
    }

    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(this.locktime)
    bb.putInt(CryptoUtil.SIGHASH_ALL)

    val s = bb.array().slice(0, bb.position())
    CryptoUtil.doubleSha256(s)
  }

  def verifyInput(index: Int): Boolean = {

    val txIn = this.ins(index)
    val script_pub = txIn.script_pubkey

    var redeem_script: Option[Script] = None
    if (Script.is_p2sh_script_pubkey(script_pub.elems)) {
      val elem = txIn.scriptSig.elems.last
      val raw_redeem = Array.concat(VarInt.toVarint(elem.data.length), elem.data)
      redeem_script = Some(Script.parse(raw_redeem.iterator))
    }

    val z = sigHash(index, redeem_script)
    val combined = script_pub+txIn.scriptSig
    combined.evaluate(z)
  }

  def verify: Boolean = {
    if (this.fee < 0)
      return false

    for (i <- 0 until ins.length) {
      if (!verifyInput(i))
        return false
    }

    return true
  }

  def singInput(index: Int, privateKey: PrivateKey): Boolean = {
    val z = sigHash(index)
    val der = privateKey.sign(z).der()

    val sig = Array.concat(der, Array(CryptoUtil.SIGHASH_ALL.asInstanceOf[Byte]))
    val sec = privateKey.point.sec()

    ins(index).scriptSig = new Script(List(ScriptElement.create(sig),ScriptElement.create(sec)))

    this.verifyInput(index)
  }

  def isCoinbase: Boolean = {
    if (this.ins.length != 1)
      return false
    if (this.ins(0).prevTx != CryptoUtil.bytesToHex(Array.ofDim(32)))
      return false
    if (this.ins(0).prevIdx != 0xffffffff)
      return false
    true
  }

}

object Tx {
  def parse(it: Iterator[Byte]): Tx = {

    var version = LeConverter.readLongLE(it, 4).asInstanceOf[Int]
    var num_in = VarInt.fromVarint(it)

    var ins = new Array[TxIn](num_in.toInt)
    for (i <- 0 until num_in.toInt) {
      ins(i) = TxIn.parse(it)
    }

    var num_out = VarInt.fromVarint(it)
    var outs = new Array[TxOut](num_out.toInt)
    for (i <- 0 until num_out.toInt) {
      outs(i) = TxOut.parse(it)
    }

    var locktime = LeConverter.readLongLE(it, 4).toInt
    new Tx(version, num_in, ins, num_out, outs, locktime)
  }
}
