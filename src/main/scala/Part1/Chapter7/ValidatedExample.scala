package Part1.Chapter7

import cats.Applicative
import cats.data.Validated
import cats.instances._
import cats.implicits._

import Print.PrintSyntax._

object ValidatedExample extends App {

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) => (accum, func(item)).mapN(_ :+ _) }
  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  type ErrorsOr[A] = Validated[List[String], A]

  def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  process(List(2, 4, 6)).print
  process(List(1, 2, 3)).print
}
