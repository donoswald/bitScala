package donmiguel

case class Tx(version: Int, num_inputs: Long, ins: Array[TxIn], num_outs: Long, outs: Array[TxOut], locktime: Int) {


}

object Tx {
  def parse(in: Array[Byte]): Tx = {

    var it = in.iterator

    var version = LeConverter.readLongLE(it, 4, 0).asInstanceOf[Int]


    var num_in = VarInt.fromVarint(it)

    var ins = new Array[TxIn](num_in.toInt)
    for (i <- 0 to num_in.toInt - 1) {
      ins(i) = TxIn.parse(it)
    }
    new Tx(version, num_in, ins, 0, null, 0)
  }
}
