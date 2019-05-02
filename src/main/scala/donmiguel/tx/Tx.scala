package donmiguel.tx

import java.nio.{ByteBuffer, ByteOrder}

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

  def serialize:Array[Byte]={
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(this.version)
      .order(ByteOrder.BIG_ENDIAN)
      .put(VarInt.toVarint(this.ins.length))
    for(txIn<-ins){
      bb.put(txIn.serialize)
    }
    bb.put(VarInt.toVarint(this.outs.length))
    for(txOut<-outs){
      bb.put(txOut.serialize)
    }
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(this.locktime)
    bb.array().slice(0,bb.position())
  }

  def sig_hash(input_index: Int): BigInt = {
    val bb = ByteBuffer.allocate(999999999)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(version)
      .order(ByteOrder.BIG_ENDIAN)
      .put(VarInt.toVarint(this.ins.length))

    for (i <- 0 until this.ins.length) {
      val tx_in = this.ins(i)

      if (i == input_index) {
        bb.put(new TxIn(
          tx_in.prev_tx,
          tx_in.prev_idx,
          tx_in.script_pubkey,
          tx_in.sequence
        ).serialize)
      } else {
        bb.put(new TxIn(
          tx_in.prev_tx,
          tx_in.prev_idx,
          null,
          tx_in.sequence
        ).serialize)
      }
    }
    bb.put(VarInt.toVarint(this.outs.length))
    for(txOut<- outs){
      bb.put(txOut.serialize)
    }

    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(this.locktime)
    bb.putInt(CryptoUtil.SIGHASH_ALL)

    val s = bb.array().slice(0,bb.position())
    BigInt.apply(CryptoUtil.bytesToHex(CryptoUtil.doubleSha256(s)),16)
  }


}

object Tx {
  def parse(it: Iterator[Byte]): Tx = {

    var version = LeConverter.readLongLE(it, 4).asInstanceOf[Int]

    var num_in = VarInt.fromVarint(it)
    var ins = new Array[TxIn](num_in.toInt)
    for (i <- 0 until  num_in.toInt ) {
      ins(i) = TxIn.parse(it)
    }

    var num_out = VarInt.fromVarint(it)
    var outs = new Array[TxOut](num_out.toInt)
    for (i <- 0 until num_out.toInt ) {
      outs(i) = TxOut.parse(it)
    }

    var locktime = LeConverter.readLongLE(it, 4).toInt
    new Tx(version, num_in, ins, num_out, outs, locktime)
  }
}
