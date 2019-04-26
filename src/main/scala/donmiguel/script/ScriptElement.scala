package donmiguel.script

case class ScriptElement(opcode: Int, data: Array[Byte]) {

  def isOpcode: Boolean = opcode != null && opcode > OpCode.OP_PUSHDATA4.id

}
