package Part1.Chapter4

import cats.Eval
import Print.PrintSyntax._
import cats.data.Writer
import cats.syntax._
import cats.implicits._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

object EvalExample extends App {
  val saying = Eval.always { println("Step 1"); "The cat" }.map { str => println("Step 2"); s"$str sat on" }.memoize.map {
    str => println("Step 3"); s"$str the mat"
  }
  saying.value.print
  saying.value.print
  saying.map(_ => 10).value.print

  def factorial(n: BigInt): Eval[BigInt] =
    if (n === 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }
//  factorial(50000).value.print

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
    case ::(head, next) => Eval.defer(foldRight(next, fn(head, acc))(fn))
    case Nil            => Eval.now(acc)
  }

  foldRight(List.fill(10000)(1), 0)(_ + _).value.print

}
