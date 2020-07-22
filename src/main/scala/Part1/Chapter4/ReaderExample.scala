package Part1.Chapter4

import cats.data.Reader
import cats.implicits._

import Print.PrintSyntax._

object ReaderExample extends App {
  final case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet. $feed."

  greetAndFeed(Cat("Garfield", "lasagne")).print
  // res3: cats.package.Id[String] = "Hello Garfield. Have a nice bowlof lasagne."
  greetAndFeed(Cat("Heathcliff", "junk food")).print
  // res4: cats.package.Id[String] = "Hello Heathcliff. Have a nice bowl of junk food."

  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))
  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(db =>
    db.passwords.get(username) match {
      case Some(pass) => pass === password
      case None       => false
    }
  )

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    userOption    <- findUsername(userId)
    user          = userOption.getOrElse("")
    isCorrectPass <- checkPassword(user, password)
  } yield isCorrectPass

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )
  val db = Db(users, passwords)
  checkLogin(1, "zerocool").run(db).print
  // res7: cats.package.Id[Boolean] = true
  checkLogin(4, "davinci").run(db).print
  // res8: cats.package.Id[Boolean] = false
}
