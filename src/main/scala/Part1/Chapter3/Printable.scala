package Part1.Chapter3

import Print.PrintSyntax._

trait Printable[A] { self =>
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
}

object MainPrintable extends App {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        s"'${value}'"
    }
  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if(value) "yes" else "no"
    }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  format("hello").print
  // res2: String = "'hello'"
  format(true).print
  // res3: String = "yes"

  final case class Box[A](value: A)
  implicit def boxPrintable[A](implicit pa: Printable[A]): Printable[Box[A]] = pa.contramap(a => a.value)

  format(Box("hello world")).print
  // res4: String = "'hello world'"
  format(Box(true)).print
  // res5: String = "yes"
}
