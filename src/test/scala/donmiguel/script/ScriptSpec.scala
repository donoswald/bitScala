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

    println(script.evaluate(z))

  }

  it should "evaluate exercise" in {


    val script = new Script(List[ScriptElement](
      ScriptElement.create(OpCode.OP_6),
      ScriptElement.create(OpCode.OP_DUP),

    )
    )

    //assert(script.evaluate(z)==true)

  }

}
