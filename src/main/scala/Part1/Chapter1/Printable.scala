package Part1.Chapter1

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(a: String): String = a
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(a: Int): String = s"$a"
  }

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat): String =
      s"${Printable.format(cat.name)} is a ${Printable.format(cat.age)} year-old ${Printable.format(cat.color)} cat."
  }
}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)
  def print[A](a: A)(implicit p: Printable[A]): Unit    = println(p.format(a))
}

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit p: Printable[A]): String = p.format(a)
    def print(implicit p: Printable[A]): Unit    = Printable.print(a)
  }
}

final case class Cat(name: String, age: Int, color: String)
object PrintableMain extends App {
  val cat = Cat("Murka", 3, "ginger")

  import PrintableInstances.catPrintable
  Printable.print(cat)

  import PrintableSyntax._

  cat.print

}
