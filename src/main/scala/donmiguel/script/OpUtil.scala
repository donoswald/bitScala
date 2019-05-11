package donmiguel.script

import java.nio.ByteBuffer

import scala.collection.mutable.ListBuffer

object OpUtil extends Enumeration {


  // crypto
  // block state
  val OP_CHECKLOCKTIMEVERIFY = Value(177)
  val OP_CHECKSEQUENCEVERIFY = Value(178)

  // reserved words
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_RESERVED = Value(80)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_VER = Value(98)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = false)
  val OP_VERIF = Value(101)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = false)
  val OP_VERNOTIF = Value(102)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_RESERVED1 = Value(137)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_RESERVED2 = Value(138)
  @OpCodeReserved
  val OP_NOP1 = Value(176)
  @OpCodeReserved
  val OP_NOP4 = Value(179)
  @OpCodeReserved
  val OP_NOP5 = Value(180)
  @OpCodeReserved
  val OP_NOP6 = Value(181)
  @OpCodeReserved
  val OP_NOP7 = Value(182)
  @OpCodeReserved
  val OP_NOP8 = Value(183)
  @OpCodeReserved
  val OP_NOP9 = Value(184)
  @OpCodeReserved
  val OP_NOP10 = Value(185)

  // pseudo words
  val OP_PUBKEYHASH = Value(253)
  val OP_PUBKEY = Value(254)
  val OP_INVALIDOPCODE = Value(255)

}
