package donmiguel.script

case class ScriptElement(val opcode: Int, val data: Array[Byte] = Array[Byte]()) {

  def isOpcode: Boolean = opcode != null && opcode > OpCode.OP_PUSHDATA4

}

object ScriptElement {

  def create(opValue: OpCode): ScriptElement = {
    new ScriptElement(opValue.code)
  }

  def create(it: Iterator[Byte], opcode: Int): ScriptElement = {
    ScriptElement.create(it, opcode, opcode)
  }

  def create(it: Iterator[Byte], n: Int, opcode: Int): ScriptElement = {

    var arr = new Array[Byte](n)
    it.copyToArray(arr, 0, n)
    new ScriptElement(opcode, arr)
  }

}
