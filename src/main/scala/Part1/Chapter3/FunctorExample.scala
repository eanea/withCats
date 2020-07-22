package Part1.Chapter3

import cats.Functor
import cats.syntax._
import cats.implicits._
import Print.PrintSyntax._

sealed trait Tree[+A]                                     extends Product with Serializable
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
    }
  }
}

object FunctorExample extends App {
  val br1  = Branch(Leaf(1), Leaf(2))
  val br2  = Branch(Leaf(3), br1)
  val br3  = Branch(Leaf(4), Leaf(5))
  val root: Tree[Int] = Branch(br3, br2)

  root.map(_ * 10).print
}
