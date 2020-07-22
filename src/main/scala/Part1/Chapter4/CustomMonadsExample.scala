package Part1.Chapter4

import cats.{Applicative, Eval, Monad, Traverse}
import cats.syntax.traverse._
import cats.implicits._

object CustomMonadsExample extends App {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonadNotStackSafe: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value)         => (f(value))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = flatMap(f(a)) {
      case Left(v)  => tailRecM(v)(f)
      case Right(v) => Leaf(v)
    }

  }

  val br0 = Branch(Leaf(0), Leaf(0))
  val br1 = Branch(Leaf(1), Leaf(2))
//  val br2 = Branch(Leaf(3), Leaf(4))

  val listBr = List.fill(100)(br1)

  val tree: Tree[Int] = listBr.foldLeft(br0)((acc, br) => Branch(acc, br))

  tree.iterateUntil(a => a != 0)
}
