package donmiguel

final object Secp256k1 {
  var A = 0
  var B = 7
  var P = BigInt.apply(2).pow(256) - BigInt.apply(2).pow(32) - 977
  var N = BigInt.apply("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)
  var G = new S256Point(
    new S256Element(BigInt.apply("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798", 16)),
    new S256Element(BigInt.apply("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8", 16))
  )

  override def toString: String = s"A = $A\nB = $B\nP = $P\nN = $N\G = $G"
}
