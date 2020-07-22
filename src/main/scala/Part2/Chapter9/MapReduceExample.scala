package Part2.Chapter9

import cats.kernel.Monoid
import Print.PrintSyntax._
import cats.Foldable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import cats.implicits._
import cats.syntax._

object MapReduceExample extends App {
  def foldMap[A, B: Monoid](va: Vector[A])(f: A => B): B = va.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val batchNum = values.size / Runtime.getRuntime.availableProcessors

    Future
      .sequence(values.grouped(batchNum).map(va => Future(foldMap(va)(func))))
      .map(it => it.foldLeft(Monoid[B].empty)(Monoid[B].combine))
  }

  val result: Future[Int] =
    parallelFoldMap((-1000000 to 1000000).toVector)(identity)
  Await.result(result, 0.1.second).print

  def parallelFoldMapCats[A, B: Monoid](va: Vector[A])(func: A => B): Future[B] = {
    val batchNum = va.size / Runtime.getRuntime.availableProcessors
    va.grouped(batchNum)
      .toVector
      .traverse(bch => Future(Foldable[Vector].foldLeft(bch, Monoid[B].empty)((acc, a) => func(a) |+| acc)))
      .map(_.combineAll)
  }

  val result2: Future[Int] =
    parallelFoldMap((-1000000 to 1000000).toVector)(identity)
  Await.result(result2, 1.second).print
}
