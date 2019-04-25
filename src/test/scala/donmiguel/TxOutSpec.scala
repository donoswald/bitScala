package donmiguel

class TxOutSpec extends UnitSpec {

  it should "parse amount" in {

    var raw = "a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"

    var txOut = TxOut.parse(CryptoUtil.hexToBytes(raw).iterator)

    assert(txOut.amount ==32454049)
  }

  it should "parse script sig" in {
    var raw = "a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"

    var txOut = TxOut.parse(CryptoUtil.hexToBytes(raw).iterator)

    //TODO write a Script.serialize method
    //assert(txOut.script_pubkey.serialize()=="1976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac")

  }

}
