package donmiguel

object Main {
  def main(args: Array[String]) {



    var x = BigInt.apply("71176803099804161691761476818163093541894285134256342819133839964600195877077")
    var xx  = BigInt.apply(1,x.toByteArray)
    println(xx.toByteArray.length)
    var a = x.toByteArray
    println(a.length)

    var y  = BigInt.apply("50491828921543094065688497557819330775178480593827213618272488988709593092601")
    println(y)
    var b =y.toByteArray
    println(b.length)
    var yy = BigInt.apply(b)


  }
}
