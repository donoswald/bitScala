package donmiguel

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.ECPrivateKeyParameters
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}

class PrivateKey(val _privKey: BigInt) {

  val privKey = _privKey.bigInteger

  def sign(message: String): Signature = {
    val bytes = message.getBytes("UTF-8")
    val hash = Secp256k1.doubleDigest(bytes)

    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    val privKeyParams = new ECPrivateKeyParameters(privKey, Secp256k1.ecParams)
    signer.init(true, privKeyParams)
    val sigs = signer.generateSignature(hash)
    new Signature(BigInt.apply(sigs(0)), BigInt.apply(sigs(1)))
  }

}
