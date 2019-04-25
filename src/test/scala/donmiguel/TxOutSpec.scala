package donmiguel

class TxOutSpec extends UnitSpec {

  it should "parse amount" in {

    var raw = "a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"

    var txOut = TxOut.parse(CryptoUtil.hexToBytes(raw).iterator)

    assert(txOut.amount == 32454049)
  }

  it should "parse script sig" in {
    var raw = "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
    var tx = Tx.parse(CryptoUtil.hexToBytes(raw).iterator)

    assert(tx.outs.length == 2)

    assert(tx.outs(0).amount == 32454049)
    assert(tx.outs(0).script_pubkey.serialize.deep == CryptoUtil.hexToBytes("1976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac").deep)

    assert(tx.outs(1).amount == 10011545)
    assert(tx.outs(1).script_pubkey.serialize.deep == CryptoUtil.hexToBytes("1976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac").deep)

  }

}
