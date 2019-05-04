package donmiguel.tx

import donmiguel.UnitSpec

import scala.util.Random

class SignatureSpec extends UnitSpec {

  it should "serialize and deserializt" in {

    val r = Random.nextInt(Math.pow(2, 256).toInt)
    val s = Random.nextInt(Math.pow(2, 256).toInt)

    val original = new Signature(r, s)

    var der = original.der()

    var reconstructed = Signature.parse(original.der)

    assert(original == reconstructed)

  }

  it should "der" in {

    val testCases = Array(
      (1, 3),
      (Random.nextInt(Math.pow(2, 256).toInt), Random.nextInt(Math.pow(2, 255).toInt)),
    )

    for (test <- testCases) {
      val sig = Signature(test._1, test._2)
      val der = sig.der()
      val sig2 = Signature.parse(der)
      assert(sig == sig2)
    }

  }

}
