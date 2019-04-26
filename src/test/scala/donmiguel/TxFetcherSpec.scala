package donmiguel

class TxFetcherSpec extends UnitSpec {

  it should "read a cache file" in {
    TxFetcher.load("tx.cache")

    assert(TxFetcher.cache.size == 16)
  }

}
