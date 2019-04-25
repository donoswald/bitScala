package donmiguel

object Main {
  def main(args: Array[String]) {



    for (op <- OpCode.values) {
      println(s"name $op -> value $op.id",op.id)
    }


  }
}
