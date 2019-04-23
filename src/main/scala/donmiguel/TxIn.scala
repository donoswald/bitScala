package donmiguel

case class TxIn (prev_tx:Array[Byte],prev_idx:Int, script_sig:Array[Byte],sequence:Int) {

}
object  TxIn  {

  def parse (it:Iterator[Byte]):TxIn = {

    var tx_prev= LeConverter.readByteArrayLE(it,32,0)
    var prev_idx = VarInt.fromVarint(it)

    new TxIn(tx_prev,prev_idx.intValue(),null,0)

  }
}

