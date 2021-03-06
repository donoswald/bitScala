package donmiguel.tx

import donmiguel.UnitSpec
import donmiguel.util.CryptoUtil

class BlockSpec extends UnitSpec {

  it should "parse/serialize" in {

    val serializedBlock = "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d"
    val blockRaw = CryptoUtil.hexToBytes(serializedBlock)
    val block = Block.parse(blockRaw.iterator)

    assert(block.version.intValue() == Integer.parseInt("20000002", 16))
    assert(block.prevBlock == "000000000000000000fd0c220a0a8c3bc5a7b487e8c8de0dfa2373b12894c38e")
    assert(block.merkleRoot == "be258bfd38db61f957315c3f9e9c5e15216857398d50402d5089a8e0fc50075b")
    assert(block.timestamp.intValue() == Integer.parseInt("59a7771e", 16))
    assert(CryptoUtil.bytesToHex(block.bits) == "e93c0118")
    assert(CryptoUtil.bytesToHex(block.nonce) == "a4ffd71d")

    assert(CryptoUtil.bytesToHex(block.serialize()) == serializedBlock)
  }

  it should "hash" in {

    val serializedBlock = "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d"
    val block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)

    assert(CryptoUtil.bytesToHex(block.hash()) == "0000000000000000007e9e4c586439b0cdbe13b1370bdd9435d76a644d047523")

  }

  it should "bip9" in {
    var serializedBlock = "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d"
    var block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)

    assert(block.bip9() == true)


    serializedBlock = "0400000039fa821848781f027a2e6dfabbf6bda920d9ae61b63400030000000000000000ecae536a304042e3154be0e3e9a8220e5568c3433a9ab49ac4cbb74f8df8e8b0cc2acf569fb9061806652c27"
    block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)
    assert(block.bip9() == false)
  }

  it should "bip91" in {
    var serializedBlock = "1200002028856ec5bca29cf76980d368b0a163a0bb81fc192951270100000000000000003288f32a2831833c31a25401c52093eb545d28157e200a64b21b3ae8f21c507401877b5935470118144dbfd1"
    var block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)

    assert(block.bip91() == true)


    serializedBlock = "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d"
    block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)
    assert(block.bip91() == false)
  }

  it should "bip141" in {
    var serializedBlock = "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d"
    var block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)

    assert(block.bip141() == true)


    serializedBlock = "0000002066f09203c1cf5ef1531f24ed21b1915ae9abeb691f0d2e0100000000000000003de0976428ce56125351bae62c5b8b8c79d8297c702ea05d60feabb4ed188b59c36fa759e93c0118b74b2618"
    block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)
    assert(block.bip141() == false)
  }

  it should "target" in {
    val serializedBlock = "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d"
    val block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)

    assert("013ce9000000000000000000000000000000000000000000" == CryptoUtil.bytesToHex(block.target().toByteArray))
  }

  it should "difficutlty" in {
    val serializedBlock = "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d"
    val block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)

    assert(BigInt(888171856257L) == block.difficulty())
  }

  it should "checkPOW" in {
    var serializedBlock = "04000000fbedbbf0cfdaf278c094f187f2eb987c86a199da22bbb20400000000000000007b7697b29129648fa08b4bcd13c9d5e60abb973a1efac9c8d573c71c807c56c3d6213557faa80518c3737ec1"
    var block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)
    assert(true == block.checkPOW())

    serializedBlock = "04000000fbedbbf0cfdaf278c094f187f2eb987c86a199da22bbb20400000000000000007b7697b29129648fa08b4bcd13c9d5e60abb973a1efac9c8d573c71c807c56c3d6213557faa80518c3737ec0"
    block = Block.parse(CryptoUtil.hexToBytes(serializedBlock).iterator)
    assert(false == block.checkPOW())

  }
}
