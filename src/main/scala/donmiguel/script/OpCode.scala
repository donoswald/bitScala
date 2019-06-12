package donmiguel.script

import java.nio.ByteBuffer
import java.util

import donmiguel.crypto.S256Point
import donmiguel.tx.Signature
import donmiguel.util.CryptoUtil

import scala.collection.mutable.ListBuffer

abstract class OpCode(val code: Int) {
  OpCode.map.+=(code -> this)
}

trait SimpleOpCode {
  def execute(stack: util.LinkedList[Array[Byte]]): Boolean
}

trait IfOpCode {
  def execute(stack: util.LinkedList[Array[Byte]], ifStack: util.LinkedList[Boolean]): Boolean
}

trait AltstackOpCode {
  def execute(stack: util.LinkedList[Array[Byte]], altstack: util.LinkedList[Array[Byte]]): Boolean
}

trait SignableOpCode {
  def execute(stack: util.LinkedList[Array[Byte]], z: Array[Byte]): Boolean
}

object OpCode {
   var map = Map[Int, OpCode]()

  val NEGATIVE_ZERO = 0x80.asInstanceOf[Byte]
  val MAX_BYTE_AS_INT = 0x00ff
  val MAX_BOSITIV_BYTE = 0x7f

  val OP_DATA_MIN = 1
  val OP_DATA_MAX = 75
  val OP_PUSHDATA1 = 76
  val OP_PUSHDATA2 = 77
  val OP_PUSHDATA4 = 78

  def int2byteArr(i:Int ):Array[Byte]={
    val buf=ByteBuffer.allocate(4)
    buf.putInt(i)
    buf.array().slice(0,4)
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

  def toBool(in: Array[Byte]): Boolean = {
    for (i <- in.length - 1 to 0 by -1) {
      if (in(i) != 0)
        return !(i == 0 && (in(i) & 0xff) == 0x80)
    }
    false
  }


  def isIfClause(opValue: OpCode): Boolean = {
    opValue == OpCode.OP_IF ||
      opValue == OpCode.OP_NOTIF ||
      opValue == OpCode.OP_ELSE ||
      opValue == OpCode.OP_ENDIF
  }

  val OP_1NEGATE = new OpCode(79) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(-1))
      true
    }
    override def toString: String =  "OP_1NEGATE" +"("+this.code+")"
  }

  val OP_RESERVED = new OpCode(80) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
    override def toString: String =  "OP_RESERVED" +"("+this.code+")"
  }

  val OP_1 = new OpCode(81) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(1))
      true
    }
    override def toString: String =  "OP_1" +"("+this.code+")"
  }

  val OP_2 = new OpCode(82) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(2))
      true
    }
    override def toString: String =  "OP_2" +"("+this.code+")"
  }
  val OP_3 = new OpCode(83) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(3))
      true
    }
    override def toString: String =  "OP_3" +"("+this.code+")"
  }
  val OP_4 = new OpCode(84) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(4))
      true
    }
    override def toString: String =  "OP_4" +"("+this.code+")"
  }
  val OP_5 = new OpCode(85) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(5))
      true
    }
    override def toString: String =  "OP_5" +"("+this.code+")"
  }
  val OP_6 = new OpCode(86) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(6))
      true
    }
    override def toString: String =  "OP_6" +"("+this.code+")"
  }
  val OP_7 = new OpCode(87) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(7))
      true
    }
    override def toString: String =  "OP_7" +"("+this.code+")"
  }
  val OP_8 = new OpCode(88) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(8))
      true
    }
    override def toString: String =  "OP_8" +"("+this.code+")"
  }
  val OP_9 = new OpCode(89) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(9))
      true
    }
    override def toString: String =  "OP_9" +"("+this.code+")"
  }
  val OP_10 = new OpCode(90) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(10))
      true
    }
    override def toString: String =  "OP_10" +"("+this.code+")"
  }
  val OP_11 = new OpCode(91) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(11))
      true
    }
    override def toString: String =  "OP_11" +"("+this.code+")"
  }
  val OP_12 = new OpCode(92) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(12))
      true
    }
    override def toString: String =  "OP_12" +"("+this.code+")"
  }
  val OP_13 = new OpCode(93) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(13))
      true
    }
    override def toString: String =  "OP_13" +"("+this.code+")"
  }
  val OP_14 = new OpCode(94) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(14))
      true
    }
    override def toString: String =  "OP_14" +"("+this.code+")"
  }
  val OP_15 = new OpCode(95) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(15))
      true
    }
    override def toString: String =  "OP_15" +"("+this.code+")"
  }
  val OP_16 = new OpCode(96) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(16))
      true
    }
    override def toString: String =  "OP_16" +"("+this.code+")"
  }
  // control
  val OP_NOP = new OpCode(97) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP" +"("+this.code+")"
  }

  val OP_VER = new OpCode(98) with SimpleOpCode{
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
    override def toString: String =  "OP_VER" +"("+this.code+")"
  }

  val OP_VERIF = new OpCode(101) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
    override def toString: String =  "OP_VERIF" +"("+this.code+")"
  }

  val OP_VERNOTIF = new OpCode(102) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
    override def toString: String =  "OP_VERNOTIF" +"("+this.code+")"
  }


  val OP_VERIFY = new OpCode(105) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val elem = stack.pop()
      if (decode(elem) == 0)
        return false
      true
    }
    override def toString: String =  "OP_VERIFY" +"("+this.code+")"
  }

  val OP_IF = new OpCode(99) with IfOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]], ifStack: util.LinkedList[Boolean]): Boolean = {
      if (stack.size() < 1)
        return false

      val shouldExecute = !ifStack.contains(false)
      if (!shouldExecute) {
        ifStack.add(false)
        return false
      }

      ifStack.add(toBool(stack.poll()))
      false
    }
    override def toString: String =  "OP_IF" +"("+this.code+")"
  }
  val OP_NOTIF = new OpCode(100) with IfOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]], ifStack: util.LinkedList[Boolean]): Boolean = {
      if (stack.size() < 1)
        return false

      val shouldExecute = !ifStack.contains(false)
      if (!shouldExecute) {
        ifStack.add(false)
        return false
      }

      ifStack.add(!toBool(stack.poll()))
      false
    }
    override def toString: String =  "OP_NOTIF" +"("+this.code+")"
  }
  val OP_ELSE = new OpCode(103) with IfOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]], ifStack: util.LinkedList[Boolean]): Boolean = {
      if (ifStack.size() < 1)
        return false

      ifStack.add(!ifStack.poll())
      false
    }
    override def toString: String =  "OP_ELSE" +"("+this.code+")"
  }
  val OP_ENDIF = new OpCode(104) with IfOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]], ifStack: util.LinkedList[Boolean]): Boolean = {
      if (ifStack.size() < 1)
        return false

      ifStack.poll()
      false
    }
    override def toString: String =  "OP_ENDIF" +"("+this.code+")"
  }

  val OP_RETURN = new OpCode(106) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
    override def toString: String =  "OP_RETURN" +"("+this.code+")"
  }

  // stack ops
  val OP_TOALTSTACK = new OpCode(107) with AltstackOpCode {
    def execute(stack: util.LinkedList[Array[Byte]], altstack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      altstack.push(stack.pop())
      true
    }
    override def toString: String =  "OP_TOALTSTACK" +"("+this.code+")"
  }
  val OP_FROMALTSTACK = new OpCode(108) with AltstackOpCode {
    def execute(stack: util.LinkedList[Array[Byte]], altstack: util.LinkedList[Array[Byte]]): Boolean = {
      if (altstack.size() < 1)
        return false
      stack.push(altstack.pop())
      true
    }
    override def toString: String =  "OP_FROMALTSTACK" +"("+this.code+")"
  }
  val OP_2DROP = new OpCode(109) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      stack.pop()
      stack.pop()
      true
    }
    override def toString: String =  "OP_2DROP" +"("+this.code+")"
  }
  val OP_2DUP = new OpCode(110) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      val e0 = stack.get(0)
      val e1 = stack.get(1)
      stack.push(e1)
      stack.push(e0)
      true
    }
    override def toString: String =  "OP_2DUP" +"("+this.code+")"
  }
  val OP_3DUP = new OpCode(111) with SimpleOpCode {
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
    override def toString: String =  "OP_3DUP" +"("+this.code+")"
  }
  val OP_2OVER = new OpCode(112) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 4)
        return false
      val e2 = stack.get(2)
      val e3 = stack.get(3)
      stack.push(e3)
      stack.push(e2)
      true
    }
    override def toString: String =  "OP_2OVER" +"("+this.code+")"
  }

  val OP_2ROT = new OpCode(113) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 6)
        return false
      val e4 = stack.remove(4)
      val e5 = stack.remove(4)
      stack.push(e5)
      stack.push(e4)
      true
    }
    override def toString: String =  "OP_2ROT" +"("+this.code+")"
  }

  val OP_2SWAP = new OpCode(114) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 4)
        return false
      val e2 = stack.remove(2)
      val e3 = stack.remove(2)
      stack.push(e3)
      stack.push(e2)
      true
    }
    override def toString: String =  "OP_2SWAP" +"("+this.code+")"
  }
  val OP_IFDUP = new OpCode(115) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      if (decode(stack.peek()) != 0)
        stack.push(stack.peek())
      true
    }
    override def toString: String =  "OP_IFDUP" +"("+this.code+")"
  }
  val OP_DEPTH = new OpCode(116) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      stack.push(encode(stack.size()))
      true
    }
    override def toString: String =  "OP_DEPTH" +"("+this.code+")"
  }
  val OP_DROP = new OpCode(117) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      stack.pop()
      true
    }
    override def toString: String =  "OP_DROP" +"("+this.code+")"
  }
  val OP_DUP = new OpCode(118) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      stack.push(stack.peek())
      true
    }
    override def toString: String =  "OP_DUP" +"("+this.code+")"
  }
  val OP_NIP = new OpCode(119) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      stack.remove(1)
      true
    }
    override def toString: String =  "OP_NIP" +"("+this.code+")"
  }
  val OP_OVER = new OpCode(120) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      stack.push(stack.get(1))
      true
    }
    override def toString: String =  "OP_OVER" +"("+this.code+")"
  }
  val OP_PICK = new OpCode(121) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val n = decode(stack.pop()).asInstanceOf[Int]
      if (stack.size() < n + 1)
        return false

      stack.push(stack.get(n))
      true
    }
    override def toString: String =  "OP_PICK" +"("+this.code+")"
  }
  val OP_ROLL = new OpCode(122) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val n = decode(stack.pop()).asInstanceOf[Int]
      if (stack.size() < n + 1)
        return false

      stack.push(stack.remove(n))
      true
    }
    override def toString: String =  "OP_ROLL" +"("+this.code+")"
  }
  val OP_ROT = new OpCode(123) with SimpleOpCode {
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
    override def toString: String =  "OP_ROT" +"("+this.code+")"
  }
  val OP_SWAP = new OpCode(124) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      var e0 = stack.pop()
      var e1 = stack.pop()
      stack.push(e0)
      stack.push(e1)
      true
    }
    override def toString: String =  "OP_SWAP" +"("+this.code+")"
  }
  val OP_TUCK = new OpCode(125) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      var e0 = stack.peek()
      stack.add(2, e0)
      true
    }
    override def toString: String =  "OP_TUCK" +"("+this.code+")"
  }


  // splice ops
  val OP_CAT = new OpCode(126) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_CAT" +"("+this.code+")"
  }
  val OP_SUBSTR = new OpCode(127) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_SUBSTR" +"("+this.code+")"
  }
  val OP_LEFT = new OpCode(128) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_LEFT" +"("+this.code+")"
  }
  val OP_RIGHT = new OpCode(129) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_RIGHT" +"("+this.code+")"
  }

  val OP_SIZE = new OpCode(130) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      stack.push(encode(stack.peek().length))
      true
    }
    override def toString: String =  "OP_SIZE" +"("+this.code+")"
  }

  // bit logic
  val OP_INVERT = new OpCode(131) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_INVERT" +"("+this.code+")"
  }
  val OP_AND = new OpCode(132) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_AND" +"("+this.code+")"
  }
  val OP_OR = new OpCode(133) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_OR" +"("+this.code+")"
  }
  val OP_XOR = new OpCode(134) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_XOR" +"("+this.code+")"
  }

  val OP_EQUAL = new OpCode(135) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false
      val e0 = stack.pop()
      val e1 = stack.pop()

      if (e0.deep == e1.deep)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_EQUAL" +"("+this.code+")"
  }

  val OP_EQUALVERIFY = new OpCode(136) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      OpCode.OP_EQUAL.execute(stack) && OpCode.OP_VERIFY.execute(stack)
    }
    override def toString: String =  "OP_EQUALVERIFY" +"("+this.code+")"
  }

  val OP_RESERVED1 = new OpCode(137) with SimpleOpCode{
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
    override def toString: String =  "OP_RESERVED1" +"("+this.code+")"
  }

  val OP_RESERVED2 = new OpCode(138) with SimpleOpCode{
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = false
    override def toString: String =  "OP_RESERVED2" +"("+this.code+")"
  }


  // numeric
  val OP_1ADD = new OpCode(139) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      val n = decode(stack.pop())
      stack.push(encode(n + 1))
      true
    }
    override def toString: String =  "OP_1ADD" +"("+this.code+")"
  }

  val OP_1SUB = new OpCode(140) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false
      val n = decode(stack.pop())
      stack.push(encode(n - 1))
      true
    }
    override def toString: String =  "OP_1SUB" +"("+this.code+")"
  }

  val OP_2MUL = new OpCode(141) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_2MUL" +"("+this.code+")"
  }
  val OP_2DIV = new OpCode(142) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_2DIV" +"("+this.code+")"
  }

  val OP_NEGATE = new OpCode(143) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val n = decode(stack.pop())
      stack.push(encode(-n))
      true
    }
    override def toString: String =  "OP_NEGATE" +"("+this.code+")"
  }
  val OP_ABS = new OpCode(144) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val n = decode(stack.pop())
      stack.push(encode(if (n < 0) -n else n))
      true
    }
    override def toString: String =  "OP_ABS" +"("+this.code+")"
  }
  val OP_NOT = new OpCode(145) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      if (decode(stack.pop) == 0)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_NOT" +"("+this.code+")"
  }
  val OP_0NOTEQUAL = new OpCode(146) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      if (decode(stack.pop) == 0)
        stack.push(encode(0))
      else
        stack.push(encode(1))

      true
    }
    override def toString: String =  "OP_0NOTEQUAL" +"("+this.code+")"
  }
  val OP_ADD = new OpCode(147) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      stack.push(encode(a + b))
      true
    }
    override def toString: String =  "OP_ADD" +"("+this.code+")"
  }

  val OP_SUB = new OpCode(148) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      stack.push(encode(a - b))
      true
    }
    override def toString: String =  "OP_SUB" +"("+this.code+")"
  }

  val OP_MUL = new OpCode(149) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_MUL" +"("+this.code+")"
  }
  val OP_DIV = new OpCode(150) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_DIV" +"("+this.code+")"
  }
  val OP_MOD = new OpCode(151) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_MOD" +"("+this.code+")"
  }
  val OP_LSHIFT = new OpCode(152) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_LSHIFT" +"("+this.code+")"
  }
  val OP_RSHIFT = new OpCode(153) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = throw new NotImplementedError()
    override def toString: String =  "OP_RSHIFT" +"("+this.code+")"
  }

  val OP_BOOLAND = new OpCode(154) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = stack.pop()
      val a = stack.pop()
      if (toBool(a) && toBool(b))
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_BOOLAND" +"("+this.code+")"
  }

  val OP_BOOLOR = new OpCode(155) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = stack.pop()
      val a = stack.pop()
      if (toBool(a) || toBool(b))
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_BOOLOR" +"("+this.code+")"
  }

  val OP_NUMEQUAL = new OpCode(156) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val a = decode(stack.pop())
      val b = decode(stack.pop())
      if (a == b)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_NUMEQUAL" +"("+this.code+")"
  }

  val OP_NUMEQUALVERIFY = new OpCode(157) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {

      OP_NUMEQUAL.execute(stack) && OP_VERIFY.execute(stack)
    }
    override def toString: String =  "OP_NUMEQUALVERIFY" +"("+this.code+")"
  }

  val OP_NUMNOTEQUAL = new OpCode(158) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      if (a == b)
        stack.push(encode(0))
      else
        stack.push(encode(1))

      true
    }
    override def toString: String =  "OP_NUMNOTEQUAL" +"("+this.code+")"
  }

  val OP_LESSTHAN = new OpCode(159) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      if (a < b)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_LESSTHAN" +"("+this.code+")"
  }

  val OP_GREATERTHAN = new OpCode(160) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      if (a > b)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_GREATERTHAN" +"("+this.code+")"
  }

  val OP_LESSTHANOREQUAL = new OpCode(161) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      if (a <= b)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_LESSTHANOREQUAL" +"("+this.code+")"
  }

  val OP_GREATERTHANOREQUAL = new OpCode(162) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      if (a >= b)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_GREATERTHANOREQUAL" +"("+this.code+")"
  }

  val OP_MIN = new OpCode(163) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      if (a < b)
        stack.push(encode(a))
      else
        stack.push(encode(b))

      true
    }
    override def toString: String =  "OP_MIN" +"("+this.code+")"
  }

  val OP_MAX = new OpCode(164) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 2)
        return false

      val b = decode(stack.pop())
      val a = decode(stack.pop())
      if (a > b)
        stack.push(encode(a))
      else
        stack.push(encode(b))

      true
    }
    override def toString: String =  "OP_MAX" +"("+this.code+")"
  }

  val OP_WITHIN = new OpCode(165) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 3)
        return false

      val max = decode(stack.pop())
      val min = decode(stack.pop())
      val x = decode(stack.pop())
      if (x >= min && x < max)
        stack.push(encode(1))
      else
        stack.push(encode(0))

      true
    }
    override def toString: String =  "OP_WITHIN" +"("+this.code+")"
  }

  val OP_RIPEMD160 = new OpCode(166) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val x = stack.pop()
      stack.push(CryptoUtil.ripemd160(x))

      true
    }
    override def toString: String =  "OP_RIPEMD160" +"("+this.code+")"
  }

  val OP_SHA1 = new OpCode(167) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val x = stack.pop()
      stack.push(CryptoUtil.sha1(x))

      true
    }
    override def toString: String =  "OP_SHA1" +"("+this.code+")"
  }

  val OP_SHA256 = new OpCode(168) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val x = stack.pop()
      stack.push(CryptoUtil.sha256(x))

      true
    }
    override def toString: String =  "OP_SHA256" +"("+this.code+")"
  }

  val OP_HASH160 = new OpCode(169) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val x = stack.pop()
      stack.push(CryptoUtil.hash160(x))

      true
    }
    override def toString: String =  "OP_HASH160" +"("+this.code+")"
  }
  val OP_HASH256 = new OpCode(170) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      if (stack.size() < 1)
        return false

      val x = stack.pop()
      stack.push(CryptoUtil.doubleSha256(x))

      true
    }
    override def toString: String =  "OP_HASH256" +"("+this.code+")"
  }

  //TODO what here ?
  val OP_CODESEPARATOR = new OpCode(171) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = {
      false
    }
    override def toString: String =  "OP_CODESEPARATOR" +"("+this.code+")"
  }

  def OP_CHECKSIG = new OpCode(172) with SignableOpCode {

    def execute(stack: util.LinkedList[Array[Byte]], z: Array[Byte]): Boolean = {
      if (stack.size() < 2)
        return false

      val sec_pubkey = stack.pop()
      var der_sig = stack.pop()
      der_sig = der_sig.slice(0, der_sig.length - 1) // take off the last byte of the signature as that's the hash_type
      val sig = Signature.parse(der_sig)
      val point = S256Point.parse(sec_pubkey)

      if (point.verify(z, sig))
        stack.push(encode(1))
      else
        stack.push(encode(0))
      true
    }
    override def toString: String =  "OP_CHECKSIG" +"("+this.code+")"
  }

  val OP_CHECKSIGVERIFY = new OpCode(173) with SignableOpCode {

    def execute(stack: util.LinkedList[Array[Byte]], z: Array[Byte]): Boolean = {
      if (stack.size() < 2)
        return false

      OP_CHECKSIG.execute(stack, z) && OP_VERIFY.execute(stack)
    }
    override def toString: String =  "OP_CHECKSIGVERIFY" +"("+this.code+")"
  }

  val OP_CHECKMULTISIG = new OpCode(174) with SignableOpCode {

    def execute(stack: util.LinkedList[Array[Byte]], z: Array[Byte]): Boolean = {
      if (stack.size() < 1)
        return false

      val n = decode(stack.pop()).toInt
      if (stack.size() < n + 1)
        return false

      val pks = ListBuffer[Array[Byte]]()
      for (_ <- 0 until n) {
        pks.+=(stack.pop())
      }

      val m = decode(stack.pop()).toInt
      if (stack.size() < m)
        return false

      val signatures = ListBuffer[Array[Byte]]()
      for (_ <- 0 until m) {
        val sig = stack.pop()
        signatures.+=(sig.slice(0, sig.length - 1))
      }

      //Off by one bug
      if (stack.size() > 0 && decode(stack.getFirst) == 0) {
        stack.pop()
      }

      val points = pks.map(sec => S256Point.parse(sec))
      val sigs = signatures.map(der => Signature.parse(der))

      val found = sigs.filter(sig => points.find(point => point.verify(z, sig)).isDefined)

      if (found.size == m) {
        stack.push(encode(1))
        return true
      }
      stack.push(encode(0))
      return false

    }
    override def toString: String =  "OP_CHECKMULTISIG" +"("+this.code+")"
  }

  val OP_CHECKMULTISIGVERIFY = new OpCode(175) with SignableOpCode {
    def execute(stack: util.LinkedList[Array[Byte]], z: Array[Byte]): Boolean = {
      if (stack.size() < 2)
        return false

      OP_CHECKMULTISIG.execute(stack, z) && OP_VERIFY.execute(stack)
    }
    override def toString: String =  "OP_CHECKMULTISIGVERIFY" +"("+this.code+")"
  }

  val OP_NOP1 = new OpCode(176) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP1" +"("+this.code+")"
  }

  val OP_NOP4 = new OpCode(179) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP4" +"("+this.code+")"
  }

  val OP_NOP5 = new OpCode(180) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP5" +"("+this.code+")"
  }

  val OP_NOP6 = new OpCode(181) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP6" +"("+this.code+")"
  }

  val OP_NOP7 = new OpCode(182) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP7" +"("+this.code+")"
  }

  val OP_NOP8 = new OpCode(183) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP8" +"("+this.code+")"
  }

  val OP_NOP9 = new OpCode(184) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP9" +"("+this.code+")"
  }

  val OP_NOP10 = new OpCode(185) with SimpleOpCode {
    override def execute(stack: util.LinkedList[Array[Byte]]): Boolean = true
    override def toString: String =  "OP_NOP10" +"("+this.code+")"
  }
}