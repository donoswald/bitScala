package donmiguel

import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, Security}

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.util.encoders.Hex

object Main {
  def main(args: Array[String]) {


   var i = BigInt.apply("1234777777777777777777746567456544333324644654654675667544456456668677687797")
    println(i.toByteArray.length)


  }
}
