package donmiguel.script

import java.util

import donmiguel.script
import donmiguel.util.{LeConverter, VarInt}

import scala.collection.mutable.ListBuffer

case class Script(elems: List[ScriptElement] = List.empty) {


  def serialize: Array[Byte] = {
    var arr = this.raw_serialize
    var total = arr.length

    Array.concat(new VarInt(total).serialize(), arr)
  }

  def +(other: Script): Script = {
    new script.Script(other.elems.++(this.elems))
  }

  override def toString: String = {
    var buf = new ListBuffer[String]
    for(e<- elems){
      buf+=e.toString
    }

    buf.mkString("\n")
  }

  def evaluate(z: Array[Byte]): Boolean = {

    var stack = new util.LinkedList[Array[Byte]]()
    val altStack = new util.LinkedList[Array[Byte]]()
    val ifStack = new util.LinkedList[Boolean]()

    def filterDataElems(elem: ScriptElement): Boolean = {
      if (elem.opcode.isEmpty || OpCode.map.get(elem.opcode.get).isEmpty) {
        stack.push(elem.data)
        return false // don't process this elem further
      }
      true
    }

    def checkIfStatement(op: OpCode): Boolean = {

      op match {
        case ifStatement: IfOpCode =>
          return ifStatement.execute(stack, ifStack)

        case default: OpCode =>
      }

      !ifStack.contains(false)
    }

    val elems = this.elems.to[ListBuffer]

    while (elems.nonEmpty) {

      Option.apply(elems.remove(0))
        .filter(e => filterDataElems(e))
        .map(e => OpCode.map(e.opcode.get))
        .filter(op => checkIfStatement(op))
        .foreach(op => {

          if (op == OpCode.OP_RETURN) {
            return false // ends the loop
          }

          op match {
            case simple: SimpleOpCode =>
              if (!simple.execute(stack))
                return false
            case alt: AltstackOpCode =>
              if (!alt.execute(stack, altStack))
                return false
            case signable: SignableOpCode =>
              if (!signable.execute(stack, z))
                return false
          }

        })

      if (Script.isP2shScriptPubkey(elems.toList)) {
        elems.remove(0) // opHash160
        val h160 = elems.remove(0)
        elems.remove(0) // opEqual

        val redeem_raw =  stack.getFirst
        if (!OpCode.OP_HASH160.execute(stack)) {
          return false
        }
        stack.push(h160.data)
        if (!OpCode.OP_EQUAL.execute(stack)) {
          return false
        }
        if(!OpCode.OP_VERIFY.execute(stack)){
          return false
        }
        val redeem_encoded = Array.concat(new VarInt(redeem_raw.length).serialize(), redeem_raw)
        val redeem_script = Script.parse(redeem_encoded.iterator)

        for(redeem_elem<- redeem_script.elems){
          elems.+=(redeem_elem)
        }
      }

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

  def isP2shScriptPubkey(elems: List[ScriptElement]): Boolean = {
    // returns whether this follows the OP_HASH160 <20 byte hash> OP_EQUAL pattern
    elems.size == 3 &&
      elems(0).opcode.getOrElse(None) == OpCode.OP_HASH160.code &&
      elems(1).data.length == 20 &&
      elems(2).opcode.getOrElse(None) == OpCode.OP_EQUAL.code
  }

  def parse(it: Iterator[Byte]): Script = {


    var length = VarInt.parse(it).value
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
