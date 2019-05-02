package donmiguel.script

import donmiguel.util.{LeConverter, VarInt}

import scala.collection.mutable.ListBuffer

case class Script(elems: List[ScriptElement] = List.empty) {


  def serialize: Array[Byte] = {
    var arr = this.raw_serialize
    var total = arr.length

    Array.concat(VarInt.toVarint(total), arr)
  }

  private def raw_serialize: Array[Byte] = {

    var items = new ListBuffer[Array[Byte]]
    for (elem <- elems) {

      if (elem.isOpcode) {

        items += Array(elem.opcode.asInstanceOf[Byte])
        if (elem.data != null) {
          items += elem.data
        }

      } else if (elem.data != null) {

        if (elem.opcode == null) {

          items += Array(elem.data.length.toByte)
          items += elem.data

        } else if (elem.opcode < OpCode.OP_PUSHDATA1.id) {

          items += Array(elem.opcode.asInstanceOf[Byte])
          items += elem.data

        } else if (elem.opcode == OpCode.OP_PUSHDATA1.id) {

          items += Array(elem.opcode.asInstanceOf[Byte])
          items += Array(elem.data.length.asInstanceOf[Byte])
          items += elem.data

        } else if (elem.opcode == OpCode.OP_PUSHDATA2.id) {

          items += Array(elem.opcode.asInstanceOf[Byte])
          items += LeConverter.writeLE(elem.data.length, 2)
          items += elem.data

        } else if (elem.opcode == OpCode.OP_PUSHDATA4.id) {

          items += Array(elem.opcode.asInstanceOf[Byte])
          items += LeConverter.writeLE(elem.data.length, 4)
          items += elem.data

        }
      }

    }
    return items.flatten.toArray
  }



}

object Script {
  val elementStart = 0x01
  val elementEnd = 0x4b


  def parse(it: Iterator[Byte]): Script = {


    var length = VarInt.fromVarint(it)
    var count = 0
    var cmds = ListBuffer[ScriptElement]()

    while (count < length) {

      var current = it.next().asInstanceOf[Int]
      if(current<0){
        current+=256
      }
      count += 1

      if (current >= OpCode.OP_DATA_MIN.id && current <= OpCode.OP_DATA_MAX.id) {

        var n = current
        cmds.+=(createElement(it, n, n))
        count += n

      } else if (current == OpCode.OP_PUSHDATA1.id) {

        var n = LeConverter.readLongLE(it, 1).asInstanceOf[Int]
        cmds.+=(createElement(it, n, OpCode.OP_PUSHDATA1.id))
        count += n

      } else if (current == OpCode.OP_PUSHDATA2) {

        var n = LeConverter.readLongLE(it, 2).asInstanceOf[Int]
        cmds.+=(createElement(it, n, OpCode.OP_PUSHDATA2.id))
        count += n + 2

      } else {

        if(current> OpCode.OP_PUSHDATA4.id){
          cmds.+=(createElement(it,0, current))
        }
      }

    }

    require(count == length, "count==lenght")

    new Script(cmds.toList)
  }

  private def createElement(it: Iterator[Byte], n: Int, opcode: Int): ScriptElement = {

    var arr = new Array[Byte](n)
    it.copyToArray(arr, 0, n)
    new ScriptElement(opcode, arr)
  }

}
