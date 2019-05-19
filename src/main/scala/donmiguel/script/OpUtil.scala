package donmiguel.script

object OpUtil extends Enumeration {


  // crypto
  // block state
  //TODO
  val OP_CHECKLOCKTIMEVERIFY = Value(177)
  val OP_CHECKSEQUENCEVERIFY = Value(178)

  // pseudo words
  val OP_PUBKEYHASH = Value(253)
  val OP_PUBKEY = Value(254)
  val OP_INVALIDOPCODE = Value(255)

}
