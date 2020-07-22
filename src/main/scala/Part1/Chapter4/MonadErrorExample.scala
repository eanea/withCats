package Part1.Chapter4

import cats.MonadError
import cats.implicits._
import scala.util.Try

import Print.PrintSyntax._

object MonadErrorExample extends App {
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = if (age >= 18) me.pure(age)
  else me.raiseError(new IllegalArgumentException("go home kiddy"))

  validateAdult[Try](18).print
  // res7: Try[Int] = Success(18)
  validateAdult[Try](8).print
  // res8: Try[Int] = Failure(
  //java.lang.IllegalArgumentException: Age must be greater than or equal to 18
  // )
  type ExceptionOr[A] = Either[Throwable, A]
  validateAdult[ExceptionOr](-1).print
  // res9: ExceptionOr[Int] = Left(
  //java.lang.IllegalArgumentException: Age must be greater than or equal to 18
  // )
}
