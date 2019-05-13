package donmiguel.script

import java.util

import donmiguel.script
import donmiguel.util.{LeConverter, VarInt}

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime

case class Script(elems: List[ScriptElement] = List.empty) {


  def serialize: Array[Byte] = {
    var arr = this.raw_serialize
    var total = arr.length

    Array.concat(VarInt.toVarint(total), arr)
  }

  def +(other: Script): Script = {
    new script.Script(other.elems.++(this.elems))
  }

  def evaluate(z: Array[Byte]): Boolean = {

    val stack = new util.LinkedList[Array[Byte]]()
    val altStack = new util.LinkedList[Array[Byte]]()
    val ifStack = new util.LinkedList[Boolean]()

    def filterDataElems(elem: ScriptElement): Boolean = {
      if (OpCode.map.get(elem.opcode).isEmpty) {
        stack.push(elem.data)
        return false // don't process this elem further
      }
      true
    }

    def checkIfStatement(opCode: OpCode): Boolean = {
      if (opCode.isInstanceOf[IfOpCode]) {
        return opCode.asInstanceOf[IfOpCode].execute(stack, ifStack)
      }
      !ifStack.contains(false)
    }

    val result = elems.iterator
      .filter(elem => filterDataElems(elem))
      .map(elem => OpCode.map.get(elem.opcode).get)
      .filter(opCode => checkIfStatement(opCode))
      .foreach(opCode => {

        if( runtime.universe.typeOf[OpCode].typeSymbol.asClass.annotations.size>0){
          println(runtime.universe.typeOf[OpCode].typeSymbol.asClass.annotations)
          return false
        }

        if (opCode == OpCode.OP_RETURN) {
          return false // ends the loop
        } else if (opCode.isInstanceOf[SimpleOpCode]) {
          if (!opCode.asInstanceOf[SimpleOpCode].execute(stack)) return false // ends the loop
        } else if (opCode.isInstanceOf[AltstackOpCode]) {
          if (!opCode.asInstanceOf[AltstackOpCode].execute(stack, altStack)) return false // ends the loop
        } else if (opCode.isInstanceOf[SignableOpCode]) {
          if (!opCode.asInstanceOf[SignableOpCode].execute(stack, z)) return false // ends the loop
        }

      })

    if (stack.size() == 0)
      return false

    val top = stack.pop()
    if (top == Array() || top == Array(0) || top == Array(OpCode.NEGATIVE_ZERO))
      return false

    return true
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

        } else if (elem.opcode < OpCode.OP_PUSHDATA1) {

          items += Array(elem.opcode.asInstanceOf[Byte])
          items += elem.data

        } else if (elem.opcode == OpCode.OP_PUSHDATA1) {

          items += Array(elem.opcode.asInstanceOf[Byte])
          items += Array(elem.data.length.asInstanceOf[Byte])
          items += elem.data

        } else if (elem.opcode == OpCode.OP_PUSHDATA2) {

          items += Array(elem.opcode.asInstanceOf[Byte])
          items += LeConverter.writeLE(elem.data.length, 2)
          items += elem.data

        } else if (elem.opcode == OpCode.OP_PUSHDATA4) {

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
      if (current < 0) {
        current += 256
      }
      count += 1

      if (current >= OpCode.OP_DATA_MIN && current <= OpCode.OP_DATA_MAX) {

        var n = current
        cmds.+=(ScriptElement.create(it, n, n))
        count += n

      } else if (current == OpCode.OP_PUSHDATA1) {

        var n = LeConverter.readLongLE(it, 1).asInstanceOf[Int]
        cmds.+=(ScriptElement.create(it, n, OpCode.OP_PUSHDATA1))
        count += n

      } else if (current == OpCode.OP_PUSHDATA2) {

        var n = LeConverter.readLongLE(it, 2).asInstanceOf[Int]
        cmds.+=(ScriptElement.create(it, n, OpCode.OP_PUSHDATA2))
        count += n + 2

      } else {

        if (current > OpCode.OP_PUSHDATA4) {
          cmds.+=(ScriptElement.create(it, 0, current))
        }
      }

    }

    require(count == length, "count==lenght")

    new Script(cmds.toList)
  }


}
