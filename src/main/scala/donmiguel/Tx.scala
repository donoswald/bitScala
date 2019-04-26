package donmiguel

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
