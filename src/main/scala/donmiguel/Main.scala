package donmiguel

object Main {
  def main(args: Array[String]) {

    println(new FiniteFieldElement(1, 7) + new FiniteFieldElement(3, 7))
    println(new FiniteFieldElement(3, 7) - new FiniteFieldElement(1, 7))
    println(new FiniteFieldElement(3, 7) * new FiniteFieldElement(1, 7))
    println(new FiniteFieldElement(3, 7) / new FiniteFieldElement(1, 7))
    println(new FiniteFieldElement(3, 7) * 5)
    println(new FiniteFieldElement(3, 7) * 5)
    println(new FiniteFieldElement(3, 7) ** 5)

  }
}
