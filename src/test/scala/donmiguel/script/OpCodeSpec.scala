package donmiguel.script

import java.nio.charset.Charset
import java.util

import donmiguel.UnitSpec
import donmiguel.util.CryptoUtil

class OpCodeSpec extends UnitSpec {


  it should "encode/decode  zero" in {
    assert(OpCode.decode(OpCode.encode(0)) == 0)
  }

  it should "encode/decode negative zero" in {
    assert(OpCode.decode(OpCode.encode(0x80)) == 0x80)
  }

  it should "encode/decode Byte" in {
    for (i <- 0 to 256) {
      assert(OpCode.decode(OpCode.encode(i)) == i)
    }
  }

  it should "encode/decode negative Byte" in {
    for (i <- 0 to -256 by -1) {
      assert(OpCode.decode(OpCode.encode(i)) == i)
    }
  }

  it should "encode/decode long max/min" in {
    assert(OpCode.decode(OpCode.encode(Long.MaxValue)) == Long.MaxValue)
    assert(OpCode.decode(OpCode.encode(Long.MinValue + 1)) == Long.MinValue + 1)
  }

  it should "cast to bool" in {

    assert(OpCode.toBool(OpCode.encode(0L)) == false)

    assert(OpCode.toBool(OpCode.encode(0x00)) == false)

    assert(OpCode.toBool(OpCode.encode(0x80)) == false)

    assert(OpCode.toBool(OpCode.encode(0x01)) == true)


    assert(OpCode.toBool(OpCode.encode(0xff)) == true)

    assert(OpCode.toBool(OpCode.encode(0x0080)) == false)

  }


  it should "if dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_IFDUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_IFDUP.execute(list) == true)
    assert(list.size() == 2)
    assert(list.get(1).deep == Array(0x01).deep)
  }

  it should "depth" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_DEPTH.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    assert(OpCode.OP_DEPTH.execute(list) == true)
    assert(list.size() == 2)
    assert(OpCode.decode(list.get(0)) == 1)
  }

  it should "drop" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_DROP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_DROP.execute(list) == true)
    assert(list.size() == 0)
  }

  it should "dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_DUP.execute(list) == true)
    assert(list.size() == 2)
    assert(list.get(0).deep == list.get(1).deep)
  }

  it should "nip" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_NIP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_NIP.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_NIP.execute(list) == true)
    assert(list.size() == 1)
    assert(list.get(0).deep == Array(0x02).deep)
  }

  it should "over" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_OVER.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_OVER.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_OVER.execute(list) == true)
    assert(list.size() == 3)
    assert(list.get(0).deep == Array(0x01).deep)
    assert(list.get(1).deep == Array(0x02).deep)
    assert(list.get(2).deep == Array(0x01).deep)
  }

  it should "pick" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_PICK.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_PICK.execute(list) == false)
    assert(list.isEmpty)

    list.push(Array(0x01))
    list.push(Array(0x00))
    assert(OpCode.OP_PICK.execute(list) == true)
    assert(list.size() == 2)
    assert(Array(0x01).deep == list.get(0).deep)
    assert(list.get(0).deep == list.get(1).deep)
  }

  it should "roll" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_ROLL.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_ROLL.execute(list) == false)
    assert(list.isEmpty)

    list.push(Array(0x01))
    list.push(Array(0x00))
    assert(OpCode.OP_ROLL.execute(list) == true)
    assert(list.size() == 1)
    assert(Array(0x01).deep == list.get(0).deep)
  }

  it should "rot" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_ROT.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_ROT.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_ROT.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_ROT.execute(list) == true)
    assert(list.size() == 3)

    assert(list.get(0).deep == Array(0x00).deep)
    assert(list.get(1).deep == Array(0x02).deep)
    assert(list.get(2).deep == Array(0x01).deep)

  }

  it should "swap" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_SWAP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_SWAP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_SWAP.execute(list) == true)
    assert(list.size() == 2)
    assert(list.get(0).deep == Array(0x00).deep)
    assert(list.get(1).deep == Array(0x01).deep)

  }

  it should "tuck" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_TUCK.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_TUCK.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_TUCK.execute(list) == true)
    assert(list.size() == 3)
    assert(list.get(0).deep == Array(0x01).deep)
    assert(list.get(1).deep == Array(0x00).deep)
    assert(list.get(2).deep == Array(0x01).deep)

  }


  it should "2 drop" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_2DROP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_2DROP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_2DROP.execute(list) == true)
    assert(list.size() == 0)

  }

  it should "2 dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_2DUP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_2DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_2DUP.execute(list) == true)
    assert(list.size() == 4)
    assert(list.get(0).deep == Array(0x01).deep)
    assert(list.get(1).deep == Array(0x00).deep)
    assert(list.get(2).deep == Array(0x01).deep)
    assert(list.get(3).deep == Array(0x00).deep)

  }

  it should "3 dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_3DUP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_3DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_3DUP.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_3DUP.execute(list) == true)
    assert(list.size() == 6)
    assert(list.get(0).deep == Array(0x02).deep)
    assert(list.get(1).deep == Array(0x01).deep)
    assert(list.get(2).deep == Array(0x00).deep)
    assert(list.get(3).deep == Array(0x02).deep)
    assert(list.get(4).deep == Array(0x01).deep)
    assert(list.get(5).deep == Array(0x00).deep)

  }

  it should "2 over" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_2OVER.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_2OVER.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_2OVER.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_2OVER.execute(list) == false)

    list.push(Array(0x03))
    assert(OpCode.OP_2OVER.execute(list) == true)

    assert(list.size() == 6)
    assert(list.get(0).deep == Array(0x01).deep)
    assert(list.get(1).deep == Array(0x00).deep)
    assert(list.get(2).deep == Array(0x03).deep)
    assert(list.get(3).deep == Array(0x02).deep)
    assert(list.get(4).deep == Array(0x01).deep)
    assert(list.get(5).deep == Array(0x00).deep)

  }

  it should "2 rot" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_2ROT.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_2ROT.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_2ROT.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_2ROT.execute(list) == false)

    list.push(Array(0x03))
    assert(OpCode.OP_2ROT.execute(list) == false)

    list.push(Array(0x04))
    assert(OpCode.OP_2ROT.execute(list) == false)

    list.push(Array(0x05))
    assert(OpCode.OP_2ROT.execute(list) == true)

    assert(list.size() == 6)
    assert(list.get(0).deep == Array(0x01).deep)
    assert(list.get(1).deep == Array(0x00).deep)
    assert(list.get(2).deep == Array(0x05).deep)
    assert(list.get(3).deep == Array(0x04).deep)
    assert(list.get(4).deep == Array(0x03).deep)
    assert(list.get(5).deep == Array(0x02).deep)

  }

  it should "2 swap" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_2SWAP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_2SWAP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_2SWAP.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_2SWAP.execute(list) == false)

    list.push(Array(0x03))
    assert(OpCode.OP_2SWAP.execute(list) == true)
    assert(list.size() == 4)
    assert(list.get(0).deep == Array(0x01).deep)
    assert(list.get(1).deep == Array(0x00).deep)
    assert(list.get(2).deep == Array(0x03).deep)
    assert(list.get(3).deep == Array(0x02).deep)

  }

  it should "size" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_SIZE.execute(list) == false)

    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_SIZE.execute(list) == true)
    assert(OpCode.decode(list.peek()) == "Hello World".getBytes(Charset.forName("UTF-8")).length)
  }

  it should "equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_EQUAL.execute(list) == false)

    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUAL.execute(list) == false)
    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(list.get(0).deep == OpCode.encode(1).deep)
    list.clear()

    //returns true even if wrong!
    assert(OpCode.OP_EQUAL.execute(list) == false)

    list.push("x1".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUAL.execute(list) == false)
    list.push("x2".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(list.get(0).deep != OpCode.encode(1).deep)

  }

  it should "equal verify" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_EQUALVERIFY.execute(list) == false)

    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUALVERIFY.execute(list) == false)
    list.push("Hello World".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUALVERIFY.execute(list) == true)
    assert(list.size() == 0)

    // returns fals in case of different values
    assert(OpCode.OP_EQUALVERIFY.execute(list) == false)

    list.push("x1".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUALVERIFY.execute(list) == false)
    list.push("x2".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_EQUALVERIFY.execute(list) == false)
    assert(list.size() == 0)

  }

  it should "1 add" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_1ADD.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpCode.OP_1ADD.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "1 sub" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_1SUB.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpCode.OP_1SUB.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == -1)

  }

  it should "negate" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_NEGATE.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_NEGATE.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == -1)

  }

  it should "abs" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_ABS.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_ABS.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(-1))
    assert(OpCode.OP_ABS.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "not" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_NOT.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_NOT.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(0))
    assert(OpCode.OP_NOT.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "0 not equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_0NOTEQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_0NOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(0))
    assert(OpCode.OP_0NOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

  }

  it should "add" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_ADD.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_ADD.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpCode.OP_ADD.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 3)

  }

  it should "sub" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_SUB.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_SUB.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpCode.OP_SUB.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == -1)

  }

  it should "bool and" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_BOOLAND.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_BOOLAND.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpCode.OP_BOOLAND.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "bool or" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_BOOLOR.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_BOOLOR.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpCode.OP_BOOLOR.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "num equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_NUMEQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_NUMEQUAL.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpCode.OP_NUMEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(1))
    assert(OpCode.OP_NUMEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

  }

  it should "num equal verify" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_NUMEQUALVERIFY.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_NUMEQUALVERIFY.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_NUMEQUALVERIFY.execute(list) == true)
    assert(list.isEmpty)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpCode.OP_NUMEQUALVERIFY.execute(list) == false)
    assert(list.isEmpty)

  }

  it should "num not equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_NUMNOTEQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_NUMNOTEQUAL.execute(list) == false)

    list.push(OpCode.encode(0))
    assert(OpCode.OP_NUMNOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(1))
    assert(OpCode.OP_NUMNOTEQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

  }

  it should "less than" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_LESSTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_LESSTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_LESSTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpCode.OP_LESSTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpCode.OP_LESSTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)
  }

  it should "greater than" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_GREATERTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_LESSTHAN.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_GREATERTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpCode.OP_GREATERTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpCode.OP_GREATERTHAN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)
  }

  it should "less than equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_LESSTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_LESSTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_LESSTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpCode.OP_LESSTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpCode.OP_LESSTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)
  }

  it should "greater than equal" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_GREATERTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_GREATERTHANOREQUAL.execute(list) == false)

    list.push(OpCode.encode(1))
    assert(OpCode.OP_GREATERTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(1))
    list.push(OpCode.encode(0))
    assert(OpCode.OP_GREATERTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(0))
    list.push(OpCode.encode(1))
    assert(OpCode.OP_GREATERTHANOREQUAL.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)
  }

  it should "min" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_MIN.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpCode.OP_MIN.execute(list) == false)

    list.push(OpCode.encode(3))
    assert(OpCode.OP_MIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 2)

    list.clear()
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(2))
    assert(OpCode.OP_MIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 2)

  }

  it should "max" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_MAX.execute(list) == false)

    list.push(OpCode.encode(2))
    assert(OpCode.OP_MAX.execute(list) == false)

    list.push(OpCode.encode(3))
    assert(OpCode.OP_MAX.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 3)

    list.clear()
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(2))
    assert(OpCode.OP_MAX.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 3)

  }

  it should "within" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_WITHIN.execute(list) == false)

    // x
    list.push(OpCode.encode(2))
    assert(OpCode.OP_WITHIN.execute(list) == false)
    // min
    list.push(OpCode.encode(3))
    assert(OpCode.OP_WITHIN.execute(list) == false)
    // max
    list.push(OpCode.encode(4))
    assert(OpCode.OP_WITHIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

    list.clear()
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(4))
    assert(OpCode.OP_WITHIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 1)

    list.clear()
    list.push(OpCode.encode(4))
    list.push(OpCode.encode(3))
    list.push(OpCode.encode(4))
    assert(OpCode.OP_WITHIN.execute(list) == true)
    assert(list.size() == 1)
    assert(OpCode.decode(list.get(0)) == 0)

  }

  it should "ripmed 160" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_RIPEMD160.execute(list) == false)

    list.add("abc".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_RIPEMD160.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc")

  }

  it should "sha 1" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_SHA1.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_SHA1.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "11f6ad8ec52a2984abaafd7c3b516503785c2072")

  }

  it should "sha 256" in {
    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_SHA256.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_SHA256.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881")

  }

  it should "hash 160" in {
    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_HASH160.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_HASH160.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "4e944b03e84fdc97f2fb68cb62b73d000ef5be71")

  }

  it should "double hash sha 256" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_HASH256.execute(list) == false)

    list.add("x".getBytes(Charset.forName("UTF-8")))
    assert(OpCode.OP_HASH256.execute(list) == true)
    assert(list.size() == 1)
    assert(CryptoUtil.bytesToHex(list.get(0)) == "0a325ca303eb3014c43ae004970f343634db176fa1697bcc8c9efac94626488d")

  }

  it should "checksig" in {

    var z = CryptoUtil.hexToBytes("7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d")
    var sec = CryptoUtil.hexToBytes("04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")
    var sig = CryptoUtil.hexToBytes("3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601")

    val stack = new util.LinkedList[Array[Byte]]()
    stack.push(sig)
    stack.push(sec)

    assert(OpCode.OP_CHECKSIG.execute(stack, z) == true)
    assert(stack.size() == 1)
    assert(stack.getFirst.deep == OpCode.encode(1).deep)

  }

  it should "checksig verify" in {

    var z = CryptoUtil.hexToBytes("7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d")
    var sec = CryptoUtil.hexToBytes("04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")
    var sig = CryptoUtil.hexToBytes("3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601")

    val stack = new util.LinkedList[Array[Byte]]()
    stack.push(sig)
    stack.push(sec)

    assert(OpCode.OP_CHECKSIGVERIFY.execute(stack, z) == true)
    assert(stack.isEmpty)

  }

  it should "multisig" in {
    val z = CryptoUtil.hexToBytes("e71bfa115715d6fd33796948126f40a8cdd39f187e4afb03896795189fe1423c")
    val sig1 = CryptoUtil.hexToBytes("3045022100dc92655fe37036f47756db8102e0d7d5e28b3beb83a8fef4f5dc0559bddfb94e02205a36d4e4e6c7fcd16658c50783e00c341609977aed3ad00937bf4ee942a8993701")
    val sig2 = CryptoUtil.hexToBytes("3045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8eef53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e75402201")
    val sec1 = CryptoUtil.hexToBytes("022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb70")
    val sec2 = CryptoUtil.hexToBytes("03b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb71")

    val stack = new util.LinkedList[Array[Byte]]()
    stack.push(Array(0x00))
    stack.push(sig1)
    stack.push(sig2)
    stack.push(Array(0x02))
    stack.push(sec1)
    stack.push(sec2)
    stack.push(Array(0x02))

    assert(OpCode.OP_CHECKMULTISIG.execute(stack, z) == true)
    assert(stack.size() == 1)
    assert(OpCode.decode(stack.get(0)) == 1)

  }
}
