package donmiguel.script

import java.nio.charset.Charset
import java.util

import donmiguel.UnitSpec
import donmiguel.util.CryptoUtil

class OpValueSpec extends UnitSpec{

  it should "if dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_IFDUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_IFDUP.execute(list) == true)
    assert(list.size() == 2)
    assert(list.get(1).deep == Array(0x01).deep)
  }

  it should "depth" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_DEPTH.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    assert(OpValue.OP_DEPTH.execute(list) == true)
    assert(list.size() == 2)
    assert(OpCode.decode(list.get(0)) == 1)
  }

  it should "drop" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_DROP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_DROP.execute(list) == true)
    assert(list.size() == 0)
  }

  it should "dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_DUP.execute(list) == true)
    assert(list.size() == 2)
    assert(list.get(0).deep == list.get(1).deep)
  }

  it should "nip" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_NIP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_NIP.execute(list) == false)

    list.push(Array(0x02))
    assert(OpValue.OP_NIP.execute(list) == true)
    assert(list.size() == 1)
    assert(list.get(0).deep == Array(0x02).deep)
  }

  it should "over" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_OVER.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_OVER.execute(list) == false)

    list.push(Array(0x02))
    assert(OpValue.OP_OVER.execute(list) == true)
    assert(list.size() == 3)
    assert(list.get(0).deep == Array(0x01).deep)
    assert(list.get(1).deep == Array(0x02).deep)
    assert(list.get(2).deep == Array(0x01).deep)
  }

  it should "pick" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_PICK.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_PICK.execute(list) == false)
    assert(list.isEmpty)

    list.push(Array(0x01))
    list.push(Array(0x00))
    assert(OpValue.OP_PICK.execute(list) == true)
    assert(list.size() == 2)
    assert(Array(0x01).deep == list.get(0).deep)
    assert(list.get(0).deep == list.get(1).deep)
  }

  it should "roll" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_ROLL.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_ROLL.execute(list) == false)
    assert(list.isEmpty)

    list.push(Array(0x01))
    list.push(Array(0x00))
    assert(OpValue.OP_ROLL.execute(list) == true)
    assert(list.size() == 1)
    assert(Array(0x01).deep == list.get(0).deep)
  }

  it should "rot" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_ROT.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_ROT.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_ROT.execute(list) == false)

    list.push(Array(0x02))
    assert(OpValue.OP_ROT.execute(list) == true)
    assert(list.size() == 3)

    assert(list.get(0).deep==Array(0x00).deep)
    assert(list.get(1).deep==Array(0x02).deep)
    assert(list.get(2).deep==Array(0x01).deep)

  }

  it should "swap" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_SWAP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_SWAP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_SWAP.execute(list) == true)
    assert(list.size() == 2)
    assert(list.get(0).deep==Array(0x00).deep)
    assert(list.get(1).deep==Array(0x01).deep)

  }

  it should "tuck" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_TUCK.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_TUCK.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_TUCK.execute(list) == true)
    assert(list.size() == 3)
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x01).deep)

  }


  it should "2 drop" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_2DROP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_2DROP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_2DROP.execute(list) == true)
    assert(list.size() == 0)

  }

  it should "2 dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_2DUP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_2DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_2DUP.execute(list) == true)
    assert(list.size() == 4)
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x01).deep)
    assert(list.get(3).deep==Array(0x00).deep)

  }

  it should "3 dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_3DUP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_3DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_3DUP.execute(list) == false)

    list.push(Array(0x02))
    assert(OpValue.OP_3DUP.execute(list) == true)
    assert(list.size() == 6)
    assert(list.get(0).deep==Array(0x02).deep)
    assert(list.get(1).deep==Array(0x01).deep)
    assert(list.get(2).deep==Array(0x00).deep)
    assert(list.get(3).deep==Array(0x02).deep)
    assert(list.get(4).deep==Array(0x01).deep)
    assert(list.get(5).deep==Array(0x00).deep)

  }

  it should "2 over" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_2OVER.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_2OVER.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_2OVER.execute(list) == false)

    list.push(Array(0x02))
    assert(OpValue.OP_2OVER.execute(list) == false)

    list.push(Array(0x03))
    assert(OpValue.OP_2OVER.execute(list) == true)

    assert(list.size() == 6)
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x03).deep)
    assert(list.get(3).deep==Array(0x02).deep)
    assert(list.get(4).deep==Array(0x01).deep)
    assert(list.get(5).deep==Array(0x00).deep)

  }

  it should "2 rot" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_2ROT.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_2ROT.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_2ROT.execute(list) == false)

    list.push(Array(0x02))
    assert(OpValue.OP_2ROT.execute(list) == false)

    list.push(Array(0x03))
    assert(OpValue.OP_2ROT.execute(list) == false)

    list.push(Array(0x04))
    assert(OpValue.OP_2ROT.execute(list) == false)

    list.push(Array(0x05))
    assert(OpValue.OP_2ROT.execute(list) == true)

    assert(list.size() == 6)
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x05).deep)
    assert(list.get(3).deep==Array(0x04).deep)
    assert(list.get(4).deep==Array(0x03).deep)
    assert(list.get(5).deep==Array(0x02).deep)

  }

  it should "2 swap" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_2SWAP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_2SWAP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_2SWAP.execute(list) == false)

    list.push(Array(0x02))
    assert(OpValue.OP_2SWAP.execute(list) == false)

    list.push(Array(0x03))
    assert(OpValue.OP_2SWAP.execute(list) == true)
    assert(list.size() == 4)
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x03).deep)
    assert(list.get(3).deep==Array(0x02).deep)

  }

  it should "size" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_SIZE.execute(list) == false)

    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_SIZE.execute(list) == true)
    assert(OpCode.decode(list.peek()) == "Hello World".getBytes(Charset.forName("UTF-8")).length)
  }

  it should "equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_EQUAL.execute(list) == false)

    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUAL.execute(list) == false)
    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(list.get(0).deep == OpCode.encode(1).deep)
    list.clear()

    //returns true even if wrong!
    assert(OpValue.OP_EQUAL.execute(list) == false)

    list.push("x1".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUAL.execute(list) == false)
    list.push("x2".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(list.get(0).deep != OpCode.encode(1).deep)

  }

  it should "equal verify" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_EQUALVERIFY.execute(list) == false)

    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUALVERIFY.execute(list) == false)
    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUALVERIFY.execute(list) == true)
    assert(list.size() == 0)

    // returns fals in case of different values
    assert(OpValue.OP_EQUALVERIFY.execute(list) == false)

    list.push("x1".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUALVERIFY.execute(list) == false)
    list.push("x2".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_EQUALVERIFY.execute(list) == false)
    assert(list.size() == 0)

  }

  it should "1 add" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_1ADD.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpValue.OP_1ADD.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "1 sub" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_1SUB.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpValue.OP_1SUB.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == -1)

  }

  it should "negate" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_NEGATE.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_NEGATE.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == -1)

  }

  it should "abs" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_ABS.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_ABS.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(-1))
    assert(OpValue.OP_ABS.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "not" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_NOT.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_NOT.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(0))
    assert(OpValue.OP_NOT.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "0 not equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_0NOTEQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_0NOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(0))
    assert(OpValue.OP_0NOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

  }

  it should "add" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_ADD.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_ADD.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpValue.OP_ADD.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 3)

  }

  it should "sub" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_SUB.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_SUB.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpValue.OP_SUB.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == -1)

  }

  it should "bool and" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_BOOLAND.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_BOOLAND.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpValue.OP_BOOLAND.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "bool or" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_BOOLOR.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_BOOLOR.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpValue.OP_BOOLOR.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "num equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_NUMEQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_NUMEQUAL.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpValue.OP_NUMEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(1))
    assert(OpValue.OP_NUMEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "num equal verify" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_NUMEQUALVERIFY.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_NUMEQUALVERIFY.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_NUMEQUALVERIFY.execute(list) == true)
    assert(list.isEmpty)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpValue.OP_NUMEQUALVERIFY.execute(list) == false)
    assert(list.isEmpty)

  }

  it should "num not equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_NUMNOTEQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_NUMNOTEQUAL.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpValue.OP_NUMNOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(1))
    assert(OpValue.OP_NUMNOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

  }

  it should "less than" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_LESSTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_LESSTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_LESSTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpValue.OP_LESSTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpValue.OP_LESSTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)
  }

  it should "greater than" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_GREATERTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_LESSTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_GREATERTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpValue.OP_GREATERTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpValue.OP_GREATERTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)
  }

  it should "less than equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_LESSTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_LESSTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_LESSTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpValue.OP_LESSTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpValue.OP_LESSTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)
  }

  it should "greater than equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_GREATERTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_GREATERTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpValue.OP_GREATERTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpValue.OP_GREATERTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpValue.OP_GREATERTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)
  }

  it should "min" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_MIN.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpValue.OP_MIN.execute(list) == false)

    list.push(OpCode.encode(3))
    assert(OpValue.OP_MIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 2)

    list.clear()
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(2))
    assert(OpValue.OP_MIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 2)

  }

  it should "max" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_MAX.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpValue.OP_MAX.execute(list) == false)

    list.push(OpCode.encode(3))
    assert(OpValue.OP_MAX.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 3)

    list.clear()
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(2))
    assert(OpValue.OP_MAX.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 3)

  }

  it should "within" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_WITHIN.execute(list) == false)

    // x
    list.push(OpCode.encode(2))
    assert(OpValue.OP_WITHIN.execute(list) == false)
    // min
    list.push(OpCode.encode(3))
    assert(OpValue.OP_WITHIN.execute(list) == false)
    // max
    list.push(OpCode.encode(4))
    assert(OpValue.OP_WITHIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(4))
    assert(OpValue.OP_WITHIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(4))
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(4))
    assert(OpValue.OP_WITHIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

  }

  it should "ripmed 160" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_RIPEMD160.execute(list) == false)

    list.add("abc".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_RIPEMD160.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc")

  }

  it should "sha 1" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_SHA1.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_SHA1.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "11f6ad8ec52a2984abaafd7c3b516503785c2072")

  }

  it should "sha 256" in {
    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_SHA256.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_SHA256.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881")

  }

  it should "hash 160" in {
    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_HASH160.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_HASH160.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "4e944b03e84fdc97f2fb68cb62b73d000ef5be71")

  }

  it should "double hash sha 256" in{

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_HASH256.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpValue.OP_HASH256.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "0a325ca303eb3014c43ae004970f343634db176fa1697bcc8c9efac94626488d")


  }



}
