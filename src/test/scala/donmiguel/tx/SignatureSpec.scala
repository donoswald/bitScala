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

}
