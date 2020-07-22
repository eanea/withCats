package Part1.Chapter5

import cats.data.{EitherT, Writer}
import cats.instances._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import Print.PrintSyntax._

import cats.implicits._

object MonadTransformers extends App {

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] =
    EitherT.fromOption[Future](powerLevels.get(autobot), s"$autobot is unreachable.")

//  getPowerLevel("Bumblebee").print

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    p1 <- getPowerLevel(ally1)
    p2 <- getPowerLevel(ally2)
  } yield if (p1 + p2 > 15) true else false

  def tacticalReport(ally1: String, ally2: String): String = {
    val future = {
      for {
        ready <- canSpecialMove(ally1, ally2)
      } yield if (ready) s"$ally1 and $ally2 are ready to roll out!" else s"$ally1 and $ally2 need a recharge."
    }.value
    Await.result(future, 1.second) match {
      case Left(value)  => value
      case Right(value) => value
    }
  }

  tacticalReport("Jazz", "Bumblebee").print
  // res13: String = "Jazz and Bumblebee need a recharge."
  tacticalReport("Bumblebee", "Hot Rod").print
  // res14: String = "Bumblebee and Hot Rod are ready to roll out!"
  tacticalReport("Jazz", "Ironhide").print
  // res15: String = "Comms error: Ironhide unreachable"
}
