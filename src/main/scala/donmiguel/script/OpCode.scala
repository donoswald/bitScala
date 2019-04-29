package donmiguel.script

import java.util

import scala.collection.mutable.ListBuffer

object OpCode extends Enumeration {
  val NEGATIVE_ZERO = 0x80.asInstanceOf[Byte]
  val MAX_BYTE_AS_INT = 0x00ff
  val MAX_BOSITIV_BYTE = 0x7f

  abstract class OpValue(code: Int) {
    def execute(stack: util.LinkedList[Array[Byte]]): Boolean
  }

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

  def OP_0 = new OpValue(0) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(0))
      true
    }
  }

  val OP_FALSE = OP_0

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

  val OP_1NEGATE = new OpValue(79) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(-1))
      true
    }
  }
  val OP_1 = new OpValue(81) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(1))
      true
    }
  }

  val OP_TRUE = OP_1

  val OP_2 = new OpValue(82) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(2))
      true
    }
  }
  val OP_3 = new OpValue(83) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(3))
      true
    }
  }
  val OP_4 = new OpValue(84) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(4))
      true
    }
  }
  val OP_5 = new OpValue(85) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(5))
      true
    }
  }
  val OP_6 = new OpValue(86) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(6))
      true
    }
  }
  val OP_7 = new OpValue(87) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(7))
      true
    }
  }
  val OP_8 = new OpValue(88) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(8))
      true
    }
  }
  val OP_9 = new OpValue(89) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(9))
      true
    }
  }
  val OP_10 = new OpValue(90) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(10))
      true
    }
  }
  val OP_11 = new OpValue(91) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(11))
      true
    }
  }
  val OP_12 = new OpValue(92) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(12))
      true
    }
  }
  val OP_13 = new OpValue(93) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(13))
      true
    }
  }
  val OP_14 = new OpValue(94) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(14))
      true
    }
  }
  val OP_15 = new OpValue(95) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(15))
      true
    }
  }
  val OP_16 = new OpValue(96) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(16))
      true
    }
  }
  // control
  val OP_NOP = new OpValue(97) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
  }
  val OP_VERIFY = new OpValue(105) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        false
      val elem = stack.pop()
      if (elem == 0)
        false
      true
    }
  }
  val OP_RETURN = new OpValue(106) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
  }

  val OP_IF = Value(99)
  val OP_NOTIF = Value(100)
  val OP_ELSE = Value(103)
  val OP_ENDIF = Value(104)
  // stack ops
  val OP_TOALTSTACK = new OpValue(107) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()

    def execute(stack: util.Stack[Any], altstack: util.Deque[Any]): Boolean = {
      if (stack.size() < 1)
        return false
      altstack.push(stack.pop())
      true
    }
  }
  val OP_FROMALTSTACK = new OpValue(108) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()

    def execute(stack: util.Stack[Any], altstack: util.Deque[Any]): Boolean = {
      if (altstack.size() < 1)
        return false
      stack.push(altstack.pop())
      true
    }

  }
  val OP_2DROP = new OpValue(109) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      stack.pop()
      stack.pop()
      true
    }
  }
  val OP_2DUP = new OpValue(110) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      val e0 = stack.get(0)
      val e1 = stack.get(1)
      stack.push(e1)
      stack.push(e0)
      true
    }
  }
  val OP_3DUP = new OpValue(111) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 3)
        return false
      val e0 = stack.get(0)
      val e1 = stack.get(1)
      val e2 = stack.get(2)
      stack.push(e2)
      stack.push(e1)
      stack.push(e0)
      true
    }
  }
  val OP_2OVER = new OpValue(112) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 4)
        return false
      val e2 = stack.get(2)
      val e3 = stack.get(3)
      stack.push(e3)
      stack.push(e2)
      true
    }
  }

  val OP_2ROT = new OpValue(113) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 6)
        return false
      val e4 = stack.remove(4)
      val e5 = stack.remove(4)
      stack.push(e5)
      stack.push(e4)
      true
    }
  }

  val OP_2SWAP = new OpValue(114) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 4)
        return false
      val e2 = stack.remove(2)
      val e3 = stack.remove(2)
      stack.push(e3)
      stack.push(e2)
      true
    }
  }
  val OP_IFDUP = new OpValue(115) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      if (decode(stack.peek()) != 0)
        stack.push(stack.peek())
      true
    }
  }
  val OP_DEPTH = new OpValue(116) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(stack.size()))
      true
    }
  }
  val OP_DROP = new OpValue(117) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      stack.pop()
      true
    }
  }
  val OP_DUP = new OpValue(118) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      stack.push(stack.peek())
      true
    }
  }
  val OP_NIP = new OpValue(119) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      stack.remove(1)
      true
    }
  }
  val OP_OVER = new OpValue(120) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      stack.push(stack.get(1))
      true
    }
  }
  val OP_PICK = new OpValue(121) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val n = decode(stack.pop()).asInstanceOf[Int]
      if (stack.size() < n + 1)
        return false

      stack.push(stack.get(n))
      true
    }
  }
  val OP_ROLL = new OpValue(122) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val n = decode(stack.pop()).asInstanceOf[Int]
      if (stack.size() < n + 1)
        return false

      stack.push(stack.remove(n))
      true
    }
  }
  val OP_ROT = new OpValue(123) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 3)
        return false

      val e0 = stack.pop()
      val e1 = stack.pop()
      val e2 = stack.pop()

      stack.push(e1)
      stack.push(e0)
      stack.push(e2)
      true
    }
  }
  val OP_SWAP = new OpValue(124) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      var e0 = stack.pop()
      var e1 = stack.pop()
      stack.push(e0)
      stack.push(e1)
      true
    }
  }
  val OP_TUCK = new OpValue(125) {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      var e0 = stack.peek()
      stack.add(2, e0)
      true
    }

  }


  // splice ops
  @OpCodeDisabled
  val OP_CAT = Value(126)
  @OpCodeDisabled
  val OP_SUBSTR = Value(127)
  @OpCodeDisabled
  val OP_LEFT = Value(128)
  @OpCodeDisabled
  val OP_RIGHT = Value(129)
  val OP_SIZE = Value(130)

  // bit logic
  @OpCodeDisabled
  val OP_INVERT = Value(131)
  @OpCodeDisabled
  val OP_AND = Value(132)
  @OpCodeDisabled
  val OP_OR = Value(133)
  @OpCodeDisabled
  val OP_XOR = Value(134)
  val OP_EQUAL = Value(135)
  val OP_EQUALVERIFY = Value(136)

  // numeric
  val OP_1ADD = Value(139)
  val OP_1SUB = Value(140)
  @OpCodeDisabled
  val OP_2MUL = Value(141)
  @OpCodeDisabled
  val OP_2DIV = Value(142)
  val OP_NEGATE = Value(143)
  val OP_ABS = Value(144)
  val OP_NOT = Value(145)
  val OP_0NOTEQUAL = Value(146)
  val OP_ADD = Value(147)
  val OP_SUB = Value(148)
  @OpCodeDisabled
  val OP_MUL = Value(149)
  @OpCodeDisabled
  val OP_DIV = Value(150)
  @OpCodeDisabled
  val OP_MOD = Value(151)
  @OpCodeDisabled
  val OP_LSHIFT = Value(152)
  @OpCodeDisabled
  val OP_RSHIFT = Value(153)
  val OP_BOOLAND = Value(154)
  val OP_BOOLOR = Value(155)
  val OP_NUMEQUAL = Value(156)
  val OP_NUMEQUALVERIFY = Value(157)
  val OP_NUMNOTEQUAL = Value(158)
  val OP_LESSTHAN = Value(159)
  val OP_GREATERTHAN = Value(160)
  val OP_LESSTHANOREQUAL = Value(161)
  val OP_GREATERTHANOREQUAL = Value(162)
  val OP_MIN = Value(163)
  val OP_MAX = Value(164)
  val OP_WITHIN = Value(165)

  // crypto
  val OP_RIPEMD160 = Value(166)
  val OP_SHA1 = Value(167)
  val OP_SHA256 = Value(168)
  val OP_HASH160 = Value(169)
  val OP_HASH256 = Value(170)
  val OP_CODESEPARATOR = Value(171)
  val OP_CHECKSIG = Value(172)
  val OP_CHECKSIGVERIFY = Value(173)
  val OP_CHECKMULTISIG = Value(174)
  val OP_CHECKMULTISIGVERIFY = Value(175)

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
