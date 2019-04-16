package donmiguel

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, Security}

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.util.encoders.Hex

object Main {
  def main(args: Array[String]) {
    Security.addProvider(new BouncyCastleProvider)
    var msg = "test me"

    val di = new SHA256Digest();
    di.update(msg.getBytes(StandardCharsets.UTF_8), 0, msg.getBytes(StandardCharsets.UTF_8).length)
    val b = Array.ofDim[Byte](di.getDigestSize)
    di.doFinal(b, 0)
    println(new String(Hex.encode(b)))

    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(msg.getBytes(StandardCharsets.UTF_8))
    val sha256hex = new String(Hex.encode(hash))
    println(sha256hex)

    println(new String(Hex.encode(CryptoUtil.sha256(msg.getBytes(StandardCharsets.UTF_8)))))
    println(CryptoUtil.bytesToHex(CryptoUtil.sha256(msg.getBytes(StandardCharsets.UTF_8))))
    println(CryptoUtil.bytesToHex(CryptoUtil.ripemd160(msg.getBytes(StandardCharsets.UTF_8))))


    var s = ""
    s.zipWithIndex

    //    val digest1 = MessageDigest.getInstance("RIPEMD-160")
    //    val hash1 = digest1.digest(msg.getBytes(StandardCharsets.UTF_8))
    //    val ripemed160 = new String(Hex.encode(hash1))


  }
}
