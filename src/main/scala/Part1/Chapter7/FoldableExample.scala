package Part1.Chapter7

import Print.PrintSyntax._
import cats.kernel.Monoid
import cats.implicits._

object FoldableExample extends App {
  List(1, 2, 3).foldLeft(List.empty[Int])((acc, i) => i :: acc)
  List(1, 2, 3).foldRight(List.empty[Int])(_ :: _)

  def mapList[A, B](la: List[A])(f: A => B): List[B]           = la.foldRight(List.empty[B])((a, acc) => f(a) :: acc)
  def flatMapList[A, B](la: List[A])(f: A => List[B]): List[B] = la.foldRight(List.empty[B])((a, acc) => f(a) ++ acc)
  def filterList[A](la: List[A])(f: A => Boolean): List[A] = la.foldRight(List.empty[A]) { (a, acc) =>
    if (f(a)) a :: acc
    else acc
  }
  def sumList[A: Monoid](la: List[A]): A = la.foldRight(Monoid[A].empty)(Monoid[A].combine)

  mapList(List(1, 2, 3))(_ * 2).print
  // res9: List[Int] = List(2, 4, 6)
  flatMapList(List(1, 2, 3))(a => List(a, a * 10, a * 100)).print
  // res10: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)
  filterList(List(1, 2, 3))(_ % 2 == 1).print
  // res11: List[Int] = List(1, 3)
  sumList(List(1, 2, 3)).print
  // res12: Int = 6
}
