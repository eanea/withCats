package Print

object PrintSyntax {
  implicit class Print[A](a: A) {
    def print: Unit = println(a)
  }
}
