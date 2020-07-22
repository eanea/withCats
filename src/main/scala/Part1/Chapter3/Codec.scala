package Part1.Chapter3

import Print.PrintSyntax._

trait Codec[A] { self =>
  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B): String = self.encode(enc(value))

    override def decode(value: String): B = dec(self.decode(value))
  }
}

object CodecExample extends App {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

  encode(10.10).print
  decode("123.21": String).print

  final case class Box[A](value: A)
  object Box {
    implicit def boxCodec[A](implicit ca: Codec[A]): Codec[Box[A]] = ca.imap(Box.apply, _.value)
  }

  import cats.syntax._
  import cats.Functor
  import cats.implicits._

  val func1 = (x: Int) => x.toDouble
  val func2 = (y: Double) => y * 2

  val func3a: Int => Double =
    a => func2(func1(a))
  val func3b: Int => Double =
    func2.compose(func1)

  type <=[B, A] = A => B
  type F[A]     = Double <= A

  val func2b: Double <= Double = func2
  val func3c                   = func2b.contramap(func1)
}
