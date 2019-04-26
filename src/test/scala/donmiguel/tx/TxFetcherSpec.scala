package donmiguel.tx

import donmiguel.UnitSpec

class TxFetcherSpec extends UnitSpec {

  it should "read a cache file" in {
<<<<<<< HEAD
    TxFetcher.load("tx.cache")
=======
    var txFetcher = TxFetcher.load("tx.cache")
>>>>>>> 219d8ed393df2e7d1c8fe27fbc1466459e8d48e7

    assert(TxFetcher.cache.size == 16)
  }

}
