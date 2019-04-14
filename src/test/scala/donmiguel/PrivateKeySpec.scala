package donmiguel

class PrivateKeySpec extends UnitSpec {

  it should "sign and verify" in {
    var secret = 123456

    var pk = new PrivateKey(secret)

    var m = "michi"

    var sig = pk.sign(m)

    var point = Secp256k1.G * secret

    assert(point.verify(m, sig)==true)

  }

}
