package donmiguel

class TxOutSpec extends UnitSpec {

  it should "parse amount" in {

    var raw = "a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"

    var txOut = TxOut.parse(CryptoUtil.hexToBytes(raw).iterator)

    assert(txOut.amount ==32454049)
  }

}
