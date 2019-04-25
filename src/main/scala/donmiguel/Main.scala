package donmiguel

object Main {
  def main(args: Array[String]) {

    println(LeConverter.readLongLE(CryptoUtil.hexToBytes("19430600").iterator, 4))

  }
}
