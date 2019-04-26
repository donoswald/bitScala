package donmiguel.tx

import donmiguel.script.Script
import donmiguel.util.{CryptoUtil, LeConverter}

case class TxIn(prev_tx: String, prev_idx: Int, script_sig: Script, sequence: Int) {

  def value: Long = {
    var tx = TxFetcher.cache(prev_tx)

    tx.outs(prev_idx).amount
  }

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

