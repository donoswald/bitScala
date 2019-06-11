package donmiguel.tx

import donmiguel.UnitSpec
import donmiguel.util.CryptoUtil

class P2shSpec extends UnitSpec{

  it should "p2sh" in {
    TxFetcher.load("tx.cache")

  val txRaw=    "01000000011b16e4a8af0831da62d8baae47636e49060049948a3d9b8b9b78eafb853b5a2b010000008b48304502204c3da378d8323a7233892b8050f738da69daf765ddc0d9815d9ad352286b70c2022100dff11c19338daa85ec5b124434de3b4378ebe8c56950348d5f66197af1d04f09014104910ae6c9b41b04d366ea54e920663c691843bb83ef7336cd6a0f79b0ac82ee38d2ca23a24adc2348d82c8ca13f0db885712493e89d88551118a7e80ff66ab23cffffffff01604898000000000017a914b4acb9d78d6a6256964a60484c95de490eaaae758700000000"
    val txByte  = CryptoUtil.hexToBytes(txRaw)

  val tx = Tx.parse(txByte.iterator)

   print( tx.verify)

    println(tx)
  }
}
