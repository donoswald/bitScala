package donmiguel

case class TxIn (prev_tx:Array[Byte],prev_idx:Int, script_sig:Script,sequence:Int) {

}
object  TxIn  {

  def parse (it:Iterator[Byte]):TxIn = {

    var tx_prev= LeConverter.readByteArrayLE(it,32,0)
    var prev_idx = VarInt.fromVarint(it)

    var script = Script.parse(it)

    new TxIn(tx_prev,prev_idx.intValue(),script,0)

  }
}

