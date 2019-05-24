package donmiguel.script

import java.util

import donmiguel.script
import donmiguel.util.{LeConverter, VarInt}

import scala.collection.mutable.ListBuffer

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
      if (elem.opcode.isEmpty || OpCode.map.get(elem.opcode.get).isEmpty) {
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

    val elems = this.elems.to[ListBuffer]

    while (!elems.isEmpty) {

      val e = elems.remove(0)
      Option.apply(e)
        .filter(e => filterDataElems(e))
        .map(e => OpCode.map.get(e.opcode.get).get)
        .filter(op => checkIfStatement(op))
        .map(op => {
          if (op == OpCode.OP_RETURN) {
            return false // ends the loop
          } else if (op.isInstanceOf[SimpleOpCode]) {
            if (!op.asInstanceOf[SimpleOpCode].execute(stack)) return false // ends the loop
          } else if (op.isInstanceOf[AltstackOpCode]) {
            if (!op.asInstanceOf[AltstackOpCode].execute(stack, altStack)) return false // ends the loop
          } else if (op.isInstanceOf[SignableOpCode]) {
            if (!op.asInstanceOf[SignableOpCode].execute(stack, z)) return false // ends the loop
          }
        })

    }
    

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

        items += Array(elem.opcode.get.asInstanceOf[Byte])
        if (elem.data != null) {
          items += elem.data
        }

      } else if (elem.data != null) {

        if (elem.opcode.isEmpty) {

          items += LeConverter.readByteArrayLE(Array(elem.data.length.toByte).iterator, 1, 0)
          items += elem.data


        } else {
          val opCode = elem.opcode.get
          if (opCode < OpCode.OP_PUSHDATA1) {

            items += Array(opCode.asInstanceOf[Byte])
            items += elem.data

          } else if (opCode == OpCode.OP_PUSHDATA1) {

            items += Array(opCode.asInstanceOf[Byte])
            items += Array(elem.data.length.asInstanceOf[Byte])
            items += elem.data

          } else if (opCode == OpCode.OP_PUSHDATA2) {

            items += Array(opCode.asInstanceOf[Byte])
            items += LeConverter.writeLE(elem.data.length, 2)
            items += elem.data

          } else if (opCode == OpCode.OP_PUSHDATA4) {

            items += Array(opCode.asInstanceOf[Byte])
            items += LeConverter.writeLE(elem.data.length, 4)
            items += elem.data

          }
        }
      }

    }
    return items.flatten.toArray
  }


  def is_p2pkh_script_pubkey: Boolean = {
    //Returns whether this follows the OP_DUP OP_HASH160 <20 byte hash> OP_EQUALVERIFY OP_CHECKSIG pattern.'''
    this.elems.size == 5 &&
      this.elems(0).opcode.getOrElse(None) == OpCode.OP_DUP.code &&
      this.elems(1).opcode.getOrElse(None) == OpCode.OP_HASH160.code &&
      this.elems(2).data.length == 20 &&
      this.elems(3).opcode.getOrElse(None) == OpCode.OP_EQUALVERIFY.code &&
      this.elems(4).opcode.getOrElse(None) == OpCode.OP_CHECKSIG.code
  }

  def is_p2sh_script_pubkey: Boolean = {
    // returns whether this follows the OP_HASH160 <20 byte hash> OP_EQUAL pattern
    this.elems.size == 3 &&
      this.elems(0).opcode.getOrElse(None) == OpCode.OP_HASH160.code &&
      this.elems(1).data.length == 20 &&
      this.elems(2).opcode.getOrElse(None) == OpCode.OP_EQUAL.code
  }

}

object Script {

  def p2pkh_script(h160: Array[Byte]): Script = {
    new Script(List(
      ScriptElement.create(OpCode.OP_DUP),
      ScriptElement.create(OpCode.OP_HASH160),
      ScriptElement.create(h160),
      ScriptElement.create(OpCode.OP_EQUALVERIFY),
      ScriptElement.create(OpCode.OP_CHECKSIG)
    ))
  }

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
