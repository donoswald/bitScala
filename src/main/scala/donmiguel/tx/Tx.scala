package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

import com.sun.jna.platform.win32.WinDef.UINT
import donmiguel.script.{Script, ScriptElement}
import donmiguel.util.{CryptoUtil, LeConverter, VarInt}

case class Tx(version: UINT, val numInputs:VarInt, ins: Array[TxIn], num_outs: VarInt, outs: Array[TxOut], locktime: UINT, testnet: Boolean = false) {
  def fee: Long = {

    var sumIn: Long = 0
    var sumOut: Long = 0

    for (in <- ins) {
      sumIn += in.value
    }

    for (out <- outs) {
      sumOut += out.amount
    }

    return sumIn - sumOut
  }

  def serialize: Array[Byte] = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(this.version.intValue())
      .order(ByteOrder.BIG_ENDIAN)
      .put(new VarInt(this.ins.length).serialize())
    for (txIn <- ins) {
      bb.put(txIn.serialize)
    }

    bb.put(new VarInt(this.outs.length).serialize())
    for (txOut <- outs) {
      bb.put(txOut.serialize)
    }
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(this.locktime.intValue())
    bb.array().slice(0, bb.position())
  }

  def sigHash(inputIdx: Int, redeemScript: Option[Script] = Option.empty): Array[Byte] = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(version.intValue())
      .order(ByteOrder.BIG_ENDIAN)
      .put(new VarInt(this.ins.length).serialize())

    for (i <- 0 until this.ins.length) {
      val txIn = this.ins(i)

      var script: Option[Script] = None

      if (i == inputIdx) {
        script = redeemScript.orElse(Some(txIn.scriptPubkey))
      }

      bb.put(new TxIn(
        txIn.prevTx,
        txIn.prevIdx,
        script.orNull,
        txIn.sequence
      ).serialize)

    }
    bb.put(new VarInt(this.outs.length).serialize())
    for (txOut <- outs) {
      bb.put(txOut.serialize)
    }

    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(this.locktime.intValue())
    bb.putInt(CryptoUtil.SIGHASH_ALL)

    val s = bb.array().slice(0, bb.position())
    CryptoUtil.doubleSha256(s)
  }

  def verifyInput(index: Int): Boolean = {

    val txIn = this.ins(index)
    val script_pub = txIn.scriptPubkey

    var redeem_script: Option[Script] = None
    if (Script.isP2shScriptPubkey(script_pub.elems)) {
      val elem = txIn.scriptSig.elems.last
      val raw_redeem = Array.concat(new VarInt(elem.data.length).serialize(), elem.data)
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

  def coinbaseHeight: Int = {
    if (!isCoinbase)
      return -1

    if (this.ins(0).scriptSig.elems.size == 0)
      return -1

    LeConverter.readLongLE(this.ins(0).scriptSig.elems(0).data.iterator, 4).toInt

  }

}

object Tx {
  def parse(it: Iterator[Byte]): Tx = {

    var version = new UINT(LeConverter.readLongLE(it, 4))
    var num_in = VarInt.parse(it)

    var ins = new Array[TxIn](num_in.value.asInstanceOf[Int])
    for (i <- 0 until num_in.value.asInstanceOf[Int]) {
      ins(i) = TxIn.parse(it)
    }

    var num_out = VarInt.parse(it)
    var outs = new Array[TxOut](num_out.value.asInstanceOf[Int])
    for (i <- 0 until num_out.value.asInstanceOf[Int]) {
      outs(i) = TxOut.parse(it)
    }

    var locktime = new UINT(LeConverter.readLongLE(it, 4))
    new Tx(version, num_in, ins, num_out, outs, locktime)
  }
}
