package donmiguel

import play.api.libs.json.Json

object TxFetcher {
  var cache = Map[String, Tx]()

  def load(filename: String) = {

    val stream = getClass.getClassLoader.getResourceAsStream(filename)

    val json = try {
      var jsval = Json.parse(stream)
      for (line <- jsval.toString().replace("{", "").replace("}", "").split(",")) {

        var colls = line.replace("\"", "").split(":")
        var k = colls(0)
        var raw = CryptoUtil.hexToBytes(colls(1))

        if (raw(4) == 0) {
          //TODO I don't know what Jimmy Song is doing here, but it seems to work for certain Tx's
          raw = Array.concat(raw.slice(0, 4), raw.slice(6, raw.length - 1))
          cache += (k -> Tx.parse(raw.iterator))
        } else {
          try {
            cache += (k -> Tx.parse(raw.iterator))
          } catch {
            //TODO even with Jimmies Hack above, it fails on tx = 9e067aedc661fca148e13953df75f8ca6eada9ce3b3d8d68631769ac60999156
            case e: Exception => println(k, e)
          }
        }
      }

    } finally {
      stream.close()
    }
  }
}
