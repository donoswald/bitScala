package donmiguel.script

import donmiguel.UnitSpec
import donmiguel.util.CryptoUtil

class ScriptSpec extends UnitSpec {

  it should "parse" in {
    var raw = "6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"

    var script = Script.parse(CryptoUtil.hexToBytes(raw).iterator)

    assert(CryptoUtil.bytesToHex(script.elems(0).data) == "304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601")

    assert(CryptoUtil.bytesToHex(script.elems(1).data) == "035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937")

  }

  it should "serialize" in {
    var raw = CryptoUtil.hexToBytes("6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937")
    var script = Script.parse(raw.iterator)

    assert(raw.deep == script.serialize.deep)

  }

  it should "evaluate p2pk" in {

    val z = CryptoUtil.hexToBytes("7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d")
    val sig = CryptoUtil.hexToBytes("3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601")
    val sec = CryptoUtil.hexToBytes("04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")

    val sig_elem = ScriptElement.create(sig.iterator, sig.length)
    val sec_elem = ScriptElement.create(sec.iterator, sec.length)
    val opCode = ScriptElement.create(OpCode.OP_CHECKSIG)

    val script = new Script(List[ScriptElement](sig_elem, sec_elem, opCode))

    assert(script.evaluate(z))

  }

  it should "evaluate collision" in {

    val c1 = CryptoUtil.hexToBytes("255044462d312e330a25e2e3cfd30a0a0a312030206f626a0a3c3c2f57696474682032203020522f4865696768742033203020522f547970652034203020522f537562747970652035203020522f46696c7465722036203020522f436f6c6f7253706163652037203020522f4c656e6774682038203020522f42697473506572436f6d706f6e656e7420383e3e0a73747265616d0affd8fffe00245348412d3120697320646561642121212121852fec092339759c39b1a1c63c4c97e1fffe017f46dc93a6b67e013b029aaa1db2560b45ca67d688c7f84b8c4c791fe02b3df614f86db1690901c56b45c1530afedfb76038e972722fe7ad728f0e4904e046c230570fe9d41398abe12ef5bc942be33542a4802d98b5d70f2a332ec37fac3514e74ddc0f2cc1a874cd0c78305a21566461309789606bd0bf3f98cda8044629a1")

    val c2 = CryptoUtil.hexToBytes("255044462d312e330a25e2e3cfd30a0a0a312030206f626a0a3c3c2f57696474682032203020522f4865696768742033203020522f547970652034203020522f537562747970652035203020522f46696c7465722036203020522f436f6c6f7253706163652037203020522f4c656e6774682038203020522f42697473506572436f6d706f6e656e7420383e3e0a73747265616d0affd8fffe00245348412d3120697320646561642121212121852fec092339759c39b1a1c63c4c97e1fffe017346dc9166b67e118f029ab621b2560ff9ca67cca8c7f85ba84c79030c2b3de218f86db3a90901d5df45c14f26fedfb3dc38e96ac22fe7bd728f0e45bce046d23c570feb141398bb552ef5a0a82be331fea48037b8b5d71f0e332edf93ac3500eb4ddc0decc1a864790c782c76215660dd309791d06bd0af3f98cda4bc4629b1")

    val script_pub = new Script(List[ScriptElement](
      ScriptElement.create(OpCode.OP_2DUP),
      ScriptElement.create(OpCode.OP_EQUAL),
      ScriptElement.create(OpCode.OP_NOT),
      ScriptElement.create(OpCode.OP_VERIFY),
      ScriptElement.create(OpCode.OP_SHA1),
      ScriptElement.create(OpCode.OP_SWAP),
      ScriptElement.create(OpCode.OP_SHA1),
      ScriptElement.create(OpCode.OP_EQUAL)
    )
    )

    val script_sig = new Script(List[ScriptElement](
      ScriptElement.create(c1.iterator, c1.length),
      ScriptElement.create(c2.iterator, c2.length)
    ))

    val script = script_pub+script_sig

    assert(script.evaluate(Array()))

  }

  it should "add 4 and 5" in{
    val script_pub = new Script(List[ScriptElement](
      ScriptElement.create(OpCode.OP_ADD),
      ScriptElement.create(OpCode.OP_9),
      ScriptElement.create(OpCode.OP_EQUAL),
    )
    )

    val script_sig = new Script(List[ScriptElement](
      ScriptElement.create(OpCode.OP_4),
      ScriptElement.create(OpCode.OP_5)
    ))

    val script = script_pub+script_sig

    assert(script.evaluate(Array()))

  }

}
