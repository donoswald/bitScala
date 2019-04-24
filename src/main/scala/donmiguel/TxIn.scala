package donmiguel

case class TxIn(prev_tx: String, prev_idx: Int, script_sig: Script, sequence: Int) {

}

object TxIn {

  def parse(it: Iterator[Byte]): TxIn = {

    var tx_prev = CryptoUtil.bytesToHex(LeConverter.readByteArrayLE(it, 32, 0))
    var prev_idx = LeConverter.readLongLE(it,4).toInt

    var script = Script.parse(it)

    var sequence = LeConverter.readLongLE(it,4).toInt
    new TxIn(tx_prev, prev_idx, script, sequence)

  }
}

