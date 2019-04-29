package donmiguel.script

import java.nio.charset.Charset
import java.util

import donmiguel.UnitSpec

class OpValueSpec extends UnitSpec{

  it should "ifdup" in {

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


  it should "2drop" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpValue.OP_2DROP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpValue.OP_2DROP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpValue.OP_2DROP.execute(list) == true)
    assert(list.size() == 0)

  }

  it should "2dup" in {

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

  it should "3dup" in {

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

  it should "2over" in {

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

  it should "2rot" in {

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

  it should "2swap" in {

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

  it should "cast to bool" in {

    assert(OpCode.toBool(OpCode.encode(0L)) == false)

    assert(OpCode.toBool(OpCode.encode(0x00)) == false)

    assert(OpCode.toBool(OpCode.encode(0x80)) == false)

    assert(OpCode.toBool(OpCode.encode(0x01)) == true)


    assert(OpCode.toBool(OpCode.encode(0xff)) == true)

    assert(OpCode.toBool(OpCode.encode(0x0080)) == false)

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

}
