package donmiguel.util

import java.nio.charset.Charset

import donmiguel.UnitSpec

class CryptoUtilSpec extends UnitSpec {

  it should "ripmed 160" in{

   assert( CryptoUtil.bytesToHex(CryptoUtil.ripemd160("abc".getBytes(Charset.forName("UTF-8")))) =="8eb208f7e05d987a9b044a8e98c6b087f15a0bfc")

  }

  it should "sha-1" in {
    assert(CryptoUtil.bytesToHex(CryptoUtil.sha1("x".getBytes(Charset.forName("UTF-8"))))=="11f6ad8ec52a2984abaafd7c3b516503785c2072")
  }

  it should "sha-256" in{
    assert(CryptoUtil.bytesToHex(CryptoUtil.sha256("x".getBytes(Charset.forName("UTF-8"))))=="2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881")
  }

  it should "hash 160" in{

    //TODO check double hashing
    assert(CryptoUtil.bytesToHex(CryptoUtil.sha256("x".getBytes(Charset.forName("UTF-8"))))=="2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881")
    assert(CryptoUtil.bytesToHex(CryptoUtil.ripemd160(CryptoUtil.sha256("x".getBytes(Charset.forName("UTF-8")))))=="4e944b03e84fdc97f2fb68cb62b73d000ef5be71")
    assert(CryptoUtil.bytesToHex(CryptoUtil.hash160("x".getBytes(Charset.forName("UTF-8"))))=="4e944b03e84fdc97f2fb68cb62b73d000ef5be71")

  }

  it should "double sha 256" in {

    //TODO check double hashing
    assert(CryptoUtil.bytesToHex(CryptoUtil.sha256("x".getBytes(Charset.forName("UTF-8"))))=="2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881")
    assert(CryptoUtil.bytesToHex(CryptoUtil.sha256(CryptoUtil.sha256("x".getBytes("UTF-8"))))=="0a325ca303eb3014c43ae004970f343634db176fa1697bcc8c9efac94626488d")
    assert(CryptoUtil.bytesToHex(CryptoUtil.doubleSha256("x".getBytes(Charset.forName("UTF-8"))))=="0a325ca303eb3014c43ae004970f343634db176fa1697bcc8c9efac94626488d")

  }
}
