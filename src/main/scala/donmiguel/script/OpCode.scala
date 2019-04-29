package donmiguel.script

import scala.collection.mutable.ListBuffer

object OpCode extends Enumeration {
  val NEGATIVE_ZERO = 0x80.asInstanceOf[Byte]
  val MAX_BYTE_AS_INT = 0x00ff
  val MAX_BOSITIV_BYTE = 0x7f


  def encode(num: Long): Array[Byte] = {
    require(num != null, "try to encode null")
    if (num == 0)
      return Array()

    var abs_num = num.abs
    val negative = num.signum < 0

    var buffer = ListBuffer[Byte]()
    while (abs_num != 0) {
      buffer.+=((abs_num & MAX_BYTE_AS_INT).asInstanceOf[Byte])
      abs_num >>= 8
    }

    if ((buffer.last & NEGATIVE_ZERO) != 0)
      if (negative)
        buffer.+=(NEGATIVE_ZERO)
      else
        buffer.+=(0)
    else if (negative)
      buffer(buffer.size - 1) = (buffer.last | NEGATIVE_ZERO).asInstanceOf[Byte]


    buffer.toArray
  }

  def decode(bytes: Array[Byte]): Long = {
    require(bytes != null, "try decode null")
    if (bytes.isEmpty)
      return 0

    val big_endian = bytes.reverse
    var negative: Boolean = true

    var result = 0L
    if ((big_endian(0) & NEGATIVE_ZERO) != 0) {
      negative = true
      result = big_endian(0) & MAX_BOSITIV_BYTE
    } else {
      negative = false
      result = big_endian(0)
    }

    for (i <- 1 until big_endian.size) {
      result <<= 8
      result += big_endian(i) & MAX_BYTE_AS_INT
    }

    if (negative) -result else result
  }

  def toBool(in: Array[Byte]): Boolean = {
    for (i <- in.length - 1 to 0 by -1) {
      if (in(i) != 0)
        return !(i == 0 && (in(i) & 0xff) == 0x80)
    }
    false
  }


  val OP_DATA_1 = Value(1)
  val OP_DATA_2 = Value(2)
  val OP_DATA_3 = Value(3)
  val OP_DATA_4 = Value(4)
  val OP_DATA_5 = Value(5)
  val OP_DATA_6 = Value(6)
  val OP_DATA_7 = Value(7)
  val OP_DATA_8 = Value(8)
  val OP_DATA_9 = Value(9)

  val OP_DATA_10 = Value(10)
  val OP_DATA_11 = Value(11)
  val OP_DATA_12 = Value(12)
  val OP_DATA_13 = Value(13)
  val OP_DATA_14 = Value(14)
  val OP_DATA_15 = Value(15)
  val OP_DATA_16 = Value(16)
  val OP_DATA_17 = Value(17)
  val OP_DATA_18 = Value(18)
  val OP_DATA_19 = Value(19)

  val OP_DATA_20 = Value(20)
  val OP_DATA_21 = Value(21)
  val OP_DATA_22 = Value(22)
  val OP_DATA_23 = Value(23)
  val OP_DATA_24 = Value(24)
  val OP_DATA_25 = Value(25)
  val OP_DATA_26 = Value(26)
  val OP_DATA_27 = Value(27)
  val OP_DATA_28 = Value(28)
  val OP_DATA_29 = Value(29)

  val OP_DATA_30 = Value(30)
  val OP_DATA_31 = Value(31)
  val OP_DATA_32 = Value(32)
  val OP_DATA_33 = Value(33)
  val OP_DATA_34 = Value(34)
  val OP_DATA_35 = Value(35)
  val OP_DATA_36 = Value(36)
  val OP_DATA_37 = Value(37)
  val OP_DATA_38 = Value(38)
  val OP_DATA_39 = Value(39)

  val OP_DATA_40 = Value(40)
  val OP_DATA_41 = Value(41)
  val OP_DATA_42 = Value(42)
  val OP_DATA_43 = Value(43)
  val OP_DATA_44 = Value(44)
  val OP_DATA_45 = Value(45)
  val OP_DATA_46 = Value(46)
  val OP_DATA_47 = Value(47)
  val OP_DATA_48 = Value(48)
  val OP_DATA_49 = Value(49)

  val OP_DATA_50 = Value(50)
  val OP_DATA_51 = Value(51)
  val OP_DATA_52 = Value(52)
  val OP_DATA_53 = Value(53)
  val OP_DATA_54 = Value(54)
  val OP_DATA_55 = Value(55)
  val OP_DATA_56 = Value(56)
  val OP_DATA_57 = Value(57)
  val OP_DATA_58 = Value(58)
  val OP_DATA_59 = Value(59)

  val OP_DATA_60 = Value(60)
  val OP_DATA_61 = Value(61)
  val OP_DATA_62 = Value(62)
  val OP_DATA_63 = Value(63)
  val OP_DATA_64 = Value(64)
  val OP_DATA_65 = Value(65)
  val OP_DATA_66 = Value(66)
  val OP_DATA_67 = Value(67)
  val OP_DATA_68 = Value(68)
  val OP_DATA_69 = Value(69)

  val OP_DATA_70 = Value(70)
  val OP_DATA_71 = Value(71)
  val OP_DATA_72 = Value(72)
  val OP_DATA_73 = Value(73)
  val OP_DATA_74 = Value(74)
  val OP_DATA_75 = Value(75)

  val OP_DATA_MIN = OP_DATA_1
  val OP_DATA_MAX = OP_DATA_75

  val OP_PUSHDATA1 = Value(76)
  val OP_PUSHDATA2 = Value(77)
  val OP_PUSHDATA4 = Value(78)


  val OP_IF = Value(99)
  val OP_NOTIF = Value(100)
  val OP_ELSE = Value(103)
  val OP_ENDIF = Value(104)

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
