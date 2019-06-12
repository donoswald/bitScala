package donmiguel.script

import donmiguel.util.CryptoUtil

class ScriptElement(val opcode: Option[Int], val data: Array[Byte] = Array[Byte]()) {

  def isOpcode: Boolean = opcode.isDefined && opcode.get > OpCode.OP_PUSHDATA4
   override def toString: String = {

     var s = ""
     if(this.opcode.isDefined){
       if(OpCode.map.get(this.opcode.get).isDefined){
         s+= OpCode.map.get(this.opcode.get).get.toString
       }else{
         s+="DATA_"+this.data.length+" "
       }
     }
     if (this.data.length>0){
       s += CryptoUtil.bytesToHex(this.data)
     }
     s
   }
}

object ScriptElement {

  def create(opValue: OpCode): ScriptElement = {
    new ScriptElement(Some(opValue.code))
  }

  def create(arr: Array[Byte]): ScriptElement = {
    new ScriptElement(None,arr)
  }

  def create(it: Iterator[Byte], opcode: Int): ScriptElement = {
    ScriptElement.create(it, opcode, opcode)
  }

  def create(it: Iterator[Byte], n: Int, opcode: Int): ScriptElement = {

    var arr = new Array[Byte](n)
    it.copyToArray(arr, 0, n)
    new ScriptElement(Some(opcode), arr)
  }
}
