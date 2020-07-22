package Part1.Chapter4

import cats.data.Writer
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

import Print.PrintSyntax._

object WriterExample extends App {
  type Logged[A] = Writer[List[String], A]

  def slowly[A](body: => A): A = try body
  finally Thread.sleep(100)
  def factorial(n: Int): Logged[Int] = for {
    ans <- if (n === 0) slowly(1.pure[Logged])
    else
      slowly(factorial(n - 1).map(_ * n))
    _ <- List(s"fact $n $ans").tell
  } yield ans

  factorial(5).print

  Await.result(
    Future
      .sequence(
        Vector(
          Future(factorial(5)),
          Future(factorial(5))
        )
      )
      .map(_.map(_.written)),
    5.seconds
  ).print
}
