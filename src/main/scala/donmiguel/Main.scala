package donmiguel

object Main {
  def main(args: Array[String]) {

    println(new FieldElement(1, 7) + new FieldElement(3, 7))
    println(new FieldElement(3, 7) - new FieldElement(1, 7))
    println(new FieldElement(3, 7) * new FieldElement(1, 7))
    println(new FieldElement(3, 7) / new FieldElement(1, 7))
    println(new FieldElement(3, 7) * 5)
    println(new FieldElement(3, 7) * 5)
    println(new FieldElement(3, 7) ** 5)

  }
}
