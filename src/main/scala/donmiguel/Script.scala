package donmiguel

import scala.collection.mutable.ListBuffer

class Script(var cmds: List[Array[Byte]]) {

}

object Script {
  def parse(it: Iterator[Byte]): Script = {

    var length = VarInt.fromVarint(it)
    var count = 0
    var cmds = ListBuffer[Array[Byte]]()

    while (count < length) {

      var current = it.next()
      count += 1
      var current_int = current.asInstanceOf[Int]

      if (current_int >= 1 && current_int <= 75) {

        var n = current_int
        var arr = new Array[Byte](n)
        it.copyToArray(arr, 0, n)
        cmds += arr
        count += n

      } else if (current_int == 76) {

        var n = LeConverter.readLongLE(it, 2, 0).asInstanceOf[Int]
        var arr = new Array[Byte](n)
        it.copyToArray(arr, 0, n)
        cmds += arr
        count += n

      } else if (current_int == 77) {

        var n = LeConverter.readLongLE(it, 4, 0).asInstanceOf[Int]
        var arr = new Array[Byte](n)
        it.copyToArray(arr, 0, n)
        cmds += arr
        count += n + 2

      } else {

        var op_code = current_int.asInstanceOf[Byte]

        cmds += Array(op_code)

      }

    }
    require(count == length, "count==lenght")

    new Script(cmds.toList)
  }
}
