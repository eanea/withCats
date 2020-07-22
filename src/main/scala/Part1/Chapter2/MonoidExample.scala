package Part1.Chapter2

import cats.{Monoid, Semigroup}
import cats.syntax._
import cats.implicits._
import Print.PrintSyntax._


object MonoidInstances {
  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
  }

  implicit def setSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x & y
  }

  def add(items: List[Int]): Int = items.foldLeft(Monoid[Int].empty)(_ |+| _)
  def addG[A : Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(Monoid[Double].empty, Monoid[Double].empty)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  }
}

case class Order(totalCost: Double, quantity: Double)

object MonoidMain extends App {
  val intResult = 1 |+| 2
  val someNoneResult = Option.empty[Int] |+| Option(10)
  intResult.print
  someNoneResult.print
}
