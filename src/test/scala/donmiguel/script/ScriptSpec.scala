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

}
