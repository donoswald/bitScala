package donmiguel.crypto

import donmiguel.UnitSpec
import donmiguel.tx.Signature
import donmiguel.util.CryptoUtil

class S256PointSpec extends UnitSpec {

  it should "check if N*G is infinity" in {
    var point = Secp256k1.G * Secp256k1.N
    assert(point.x == ElementNone && point.y == ElementNone)
  }

  it should "serialize in sec format" in {
    var coefficient = BigInt.apply(999).pow(3)
    var point: S256Point = Secp256k1.G * coefficient
    assert(CryptoUtil.bytesToHex(point.sec(true)) == "039d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d5")
    assert(CryptoUtil.bytesToHex(point.sec(false)) == "049d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d56fa15cc7f3d38cda98dee2419f415b7513dde1301f8643cd9245aea7f3f911f9")

    coefficient = BigInt.apply(123)
    point = Secp256k1.G * coefficient
    assert(CryptoUtil.bytesToHex(point.sec(true)) == "03a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5")
    assert(CryptoUtil.bytesToHex(point.sec(false)) == "04a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b")


    coefficient = BigInt.apply(42424242)
    point = Secp256k1.G * coefficient
    assert(CryptoUtil.bytesToHex(point.sec(true)) == "03aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e")
    assert(CryptoUtil.bytesToHex(point.sec(false)) == "04aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3")

  }


  it should "test secrets and public points" in {
    var points = Array.ofDim[BigInt](4, 3)
    points(0)(0) = 7
    points(0)(1) = BigInt.apply("5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc", 16)
    points(0)(2) = BigInt.apply("6aebca40ba255960a3178d6d861a54dba813d0b813fde7b5a5082628087264da", 16)
    points(1)(0) = 1485
    points(1)(1) = BigInt.apply("c982196a7466fbbbb0e27a940b6af926c1a74d5ad07128c82824a11b5398afda", 16)
    points(1)(2) = BigInt.apply("7a91f9eae64438afb9ce6448a1c133db2d8fb9254e4546b6f001637d50901f55", 16)
    points(2)(0) = BigInt.apply(2).pow(128)
    points(2)(1) = BigInt.apply("8f68b9d2f63b5f339239c1ad981f162ee88c5678723ea3351b7b444c9ec4c0da", 16)
    points(2)(2) = BigInt.apply("662a9f2dba063986de1d90c2b6be215dbbea2cfe95510bfdf23cbf79501fff82", 16)
    points(3)(0) = BigInt.apply(2).pow(240) + BigInt.apply(2).pow(31)
    points(3)(1) = BigInt.apply("9577ff57c8234558f293df502ca4f09cbc65a6572c842b39b366f21717945116", 16)
    points(3)(2) = BigInt.apply("10b49c67fa9365ad7b90dab070be339a1daf9052373ec30ffae4f72d5e66d053", 16)

    for (row <- points) {
      var s = row(0)
      var point = new S256Point(new S256Element(row(1)), new S256Element(row(2)))
      assert(Secp256k1.G * s == point)
    }


  }

  it should "parse " in {
    var coefficient = BigInt.apply(999).pow(3)
    var point: S256Point = Secp256k1.G * coefficient
    var arr = point.sec(false)
    assert(S256Point.parse(arr) == point)
    arr = point.sec(true)
    assert(S256Point.parse(arr) == point)

  }

  it should "generate address" in {

    var arr = Array(
      (BigInt.apply(888).pow(3), "148dY81A9BmdpMhvYEVznrM45kWN32vSCN", "mieaqB68xDCtbUBYFoUNcmZNwk74xcBfTP", true),
      (BigInt.apply(321), "1S6g2xBJSED7Qr9CYZib5f4PYVhHZiVfj", "mfx3y63A7TfTtXKkv7Y6QzsPFY6QCBCXiP", false),
      (BigInt.apply("4242424242"), "1226JSptcStqn4Yq9aAmNXdwdc2ixuH9nb", "mgY3bVusRUL6ZB2Ss999CSrGVbdRwVpM8s", false))

    for (row <- arr) {

      var point = Secp256k1.G * row._1.asInstanceOf[BigInt]

      assert(point.address(row._4.asInstanceOf[Boolean], false) == row._2)
      assert(point.address(row._4.asInstanceOf[Boolean], true) == row._3)

    }
  }

  it should "verify" in {

    var point = S256Point.parse(CryptoUtil.hexToBytes("0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"))
    var sig = Signature.parse(CryptoUtil.hexToBytes("3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed"))
    var z = CryptoUtil.hexToBytes("27e0c5994dec7824e56dec6b2fcb342eb7cdb0d0957c2fce9882f715e85d81a6")

    assert(point.verify(z, sig) == true)

    point = S256Point.parse(CryptoUtil.hexToBytes("0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"))
    sig = Signature.parse(CryptoUtil.hexToBytes("3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed"))
    z = CryptoUtil.hexToBytes("27e0c5994dec7824e56dec6b2fcb342eb7cdb0d0957c2fce9882f715e85d81a6")

    assert(point.verify(z, sig) == true)
  }

}
