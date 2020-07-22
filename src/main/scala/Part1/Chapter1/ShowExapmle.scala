package Part1.Chapter1
import cats._
import cats.syntax._
import cats.implicits._


object ShowExapmle extends App {
  implicit val catShow: Show[Cat] = Show.show(cat =>
    s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

  val cat = Cat("Murka", 2, "ginger")

  println(cat.show)
}
