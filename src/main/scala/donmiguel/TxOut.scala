package donmiguel

class TxOut(var amount: Long, script_pubkey: Script) {

}

object TxOut {

  def parse(it: Iterator[Byte]): TxOut = {

    var amount = LeConverter.readLongLE(it, 8)

    new TxOut(amount, null)
  }
}
