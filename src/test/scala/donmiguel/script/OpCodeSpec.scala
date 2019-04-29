package donmiguel.script

import java.util

import donmiguel.UnitSpec

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

  it should "ifdup" in {

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

    assert(list.get(0).deep==Array(0x00).deep)
    assert(list.get(1).deep==Array(0x02).deep)
    assert(list.get(2).deep==Array(0x01).deep)

  }

  it should "swap" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_SWAP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_SWAP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_SWAP.execute(list) == true)
    assert(list.size() == 2)
    assert(list.get(0).deep==Array(0x00).deep)
    assert(list.get(1).deep==Array(0x01).deep)

  }

  it should "tuck" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_TUCK.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_TUCK.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_TUCK.execute(list) == true)
    assert(list.size() == 3)
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x01).deep)

  }


  it should "2drop" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_2DROP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_2DROP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_2DROP.execute(list) == true)
    assert(list.size() == 0)

  }

  it should "2dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_2DUP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_2DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_2DUP.execute(list) == true)
    assert(list.size() == 4)
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x01).deep)
    assert(list.get(3).deep==Array(0x00).deep)

  }

  it should "3dup" in {

    var list = new util.LinkedList[Array[Byte]]()
    assert(OpCode.OP_3DUP.execute(list) == false)

    list.push(Array(0x00))
    assert(OpCode.OP_3DUP.execute(list) == false)

    list.push(Array(0x01))
    assert(OpCode.OP_3DUP.execute(list) == false)

    list.push(Array(0x02))
    assert(OpCode.OP_3DUP.execute(list) == true)
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
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x03).deep)
    assert(list.get(3).deep==Array(0x02).deep)
    assert(list.get(4).deep==Array(0x01).deep)
    assert(list.get(5).deep==Array(0x00).deep)

  }

  it should "2rot" in {

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
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x05).deep)
    assert(list.get(3).deep==Array(0x04).deep)
    assert(list.get(4).deep==Array(0x03).deep)
    assert(list.get(5).deep==Array(0x02).deep)

  }

  it should "2swap" in {

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
    assert(list.get(0).deep==Array(0x01).deep)
    assert(list.get(1).deep==Array(0x00).deep)
    assert(list.get(2).deep==Array(0x03).deep)
    assert(list.get(3).deep==Array(0x02).deep)

  }
}
