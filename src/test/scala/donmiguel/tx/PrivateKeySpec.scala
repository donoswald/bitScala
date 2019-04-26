package donmiguel.tx

import donmiguel.UnitSpec
import donmiguel.crypto.Secp256k1

class PrivateKeySpec extends UnitSpec {

  it should "sign and verify" in {
    var secret = 123456

    var pk = new PrivateKey(secret)

    var m = "michi"

    var sig = pk.sign(m)

    var point = Secp256k1.G * secret

    assert(point.verify(m, sig) == true)

  }

  it should "encode private key to wif" in {

    var key = BigInt.apply(2).pow(256) - BigInt.apply(2).pow(199)
    var pk = new PrivateKey(key)
    assert(pk.wif(true, false) == "L5oLkpV3aqBJ4BgssVAsax1iRa77G5CVYnv9adQ6Z87te7TyUdSC")

    key = BigInt.apply(2).pow(256) - BigInt.apply(2).pow(201)
    pk = new PrivateKey(key)
    assert(pk.wif(false, true) == "93XfLeifX7Jx7n7ELGMAf1SUR6f9kgQs8Xke8WStMwUtrDucMzn")

    key = BigInt.apply("0dba685b4511dbd3d368e5c4358a1277de9486447af7b3604a69b8d9d8b7889d", 16)
    pk = new PrivateKey(key)
    assert(pk.wif(false, false) == "5HvLFPDVgFZRK9cd4C5jcWki5Skz6fmKqi1GQJf5ZoMofid2Dty")

    key = BigInt.apply("1cca23de92fd1862fb5b76e5f4f50eb082165e5191e116c18ed1a6b24be6a53f", 16)
    pk = new PrivateKey(key)
    assert(pk.wif(true, true) == "cNYfWuhDpbNM1JWc3c6JTrtrFVxU4AGhUKgw5f93NP2QaBqmxKkg")

  }
}
