package Part1.Chapter1

import Print.PrintSyntax._

import cats.Eq
import cats.syntax._
import cats.implicits._

object EqExample extends App {
  val cat1       = Cat("Garfield", 38, "orange and black")
  val cat2       = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = new Eq[Cat] {
    override def eqv(x: Cat, y: Cat): Boolean = x.age === y.age && x.name === y.name && x.color === y.color
  }

  (cat1 === cat2).print
  (optionCat1 === optionCat2).print
  (cat1.some === optionCat1).print

}
