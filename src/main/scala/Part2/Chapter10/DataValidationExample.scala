package Part2.Chapter10

import Part2.Chapter10.Check.{CheckApply, CheckFunc}
import Part2.Chapter10.Predicate.Pure
import cats.kernel.Semigroup
import cats.implicits._
import cats.syntax._
import cats.instances._
import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import Print.PrintSyntax._

sealed trait Predicate[E, A] {
  import Part2.Chapter10.Predicate._

  def apply(a: A)(implicit eSemigroup: Semigroup[E]): Validated[E, A] = this match {
    case And(l, r) =>
      (l(a), r(a))
        .mapN((_, _) => a)
    case Or(l, r) =>
      (l(a), r(a)) match {
        case (Valid(v1), Valid(v2))     => a.valid
        case (Valid(v), Invalid(e))     => v.valid
        case (Invalid(e), Valid(v))     => v.valid
        case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
      }
    case Pure(f) => f(a)
  }

  def and(that: Predicate[E, A])(implicit eS: Semigroup[E]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A])(implicit eS: Semigroup[E]): Predicate[E, A] = Or(this, that)

  def run(a: A)(implicit eSemi: Semigroup[E]): Either[E, A] = this(a).toEither
}

object Predicate {
  case class And[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]

  case class Or[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]

  case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  def lift[E, A](e: E, pred: A => Boolean): Pure[E, A] = Pure { a => if (pred(a)) a.valid else e.invalid }

}

sealed trait Check[E, A, B] {
  import Check._

  def apply(a: A)(implicit eSemi: Semigroup[E]): Validated[E, B]
  def map[C](func: B => C): Check[E, A, C] = CheckMap[E, A, B, C](this, func)
  def flatMap[C](f: B => Check[E, A, C]): CheckFlatMap[E, A, B, C] =
    CheckFlatMap[E, A, B, C](this, f)
  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = CheckAndThen(this, that)
}

object Check {
  case class CheckApply[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit eSemi: Semigroup[E]): Validated[E, A] = p(a)
  }

  case class CheckMap[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    def apply(a: A)(implicit eSemi: Semigroup[E]): Validated[E, C] = check(a).map(func)
  }

  case class CheckFlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit eSemi: Semigroup[E]): Validated[E, C] = check(a) match {
      case Valid(b)   => func(b)(a)
      case Invalid(e) => e.invalid
    }
  }

  case class CheckAndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
    override def apply(a: A)(implicit eSemi: Semigroup[E]): Validated[E, C] = check1(a) match {
      case Valid(a)   => check2(a)
      case Invalid(e) => e.invalid
    }
  }

  case class CheckFunc[E, A, B](f: A => Validated[E, B]) extends Check[E, A, B] {
    override def apply(a: A)(implicit eSemi: Semigroup[E]): Validated[E, B] = f(a)
  }
}

object DataValidationExample extends App {
  import Part2.Chapter10.Predicate._

  type Errors = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), str => str.length > n)
  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))
  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char"), str => str.contains(char))
  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char only once"), str => str.count(c => c == char) == 1)

  def check[E, A](pred: Predicate[E, A]): Check[E, A, A]       = CheckApply(pred)
  def checkF[E, A, B](f: A => Validated[E, B]): Check[E, A, B] = CheckFunc(f)

  val usernameCheck = check(longerThan(4) and alphanumeric)

  val splitEmail = check(containsOnce('@')) map { str =>
    str.split("@") match {
      case Array(left, right) => (left, right)
    }
  }

  val checkLeft  = longerThan(0)
  val checkRight = longerThan(3) and containsOnce('.')
  val checkBothSides =
    checkF((t: (String, String)) => checkLeft(t._1) andThen (_ => checkRight(t._2)) andThen (_ => t.validNel))
  val join       = checkF((t: (String, String)) => s"${t._1}@${t._2}".validNel[String])
  val emailCheck = splitEmail andThen checkBothSides andThen join

  final case class User(username: String, email: String)
  def createUser(username: String, email: String): Validated[Errors, User] =
    (usernameCheck(username), emailCheck(email)).mapN(User)

//  createUser("Eanea", "eanea@eanea.eanea").print
//  createUser("", "dave@underscore.io@io").print

  type Result[A]          = Either[Errors, A]
  type CheckKleisli[A, B] = Kleisli[Result, A, B]
  // Create a check from a function:
  def checkKleisli[A, B](func: A => Result[B]): CheckKleisli[A, B] =
    Kleisli(func)
  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): CheckKleisli[A, A] =
    Kleisli[Result, A, A](pred.run)

  val usernameCheckKleisli = checkPred(longerThan(4) and alphanumeric)

  val splitEmailKleisli = checkPred(containsOnce('@')) andThen checkKleisli((str: String) =>
    str.split("@") match {
      case Array(left, right) => (left, right).asRight
    }
  )

  val checkLeftKleisli  = checkPred(longerThan(0))
  val checkRightKleisli = checkPred(longerThan(3) and containsOnce('.'))
  val checkBothSidesKleisli =
    checkKleisli((t: (String, String)) =>
      for {
        l <- checkLeftKleisli(t._1)
        r <- checkRightKleisli(t._2)
      } yield s"$l@$r"
    )
  val emailCheckKleisli = splitEmailKleisli andThen checkBothSidesKleisli

  def createUserKleisli(username: String, email: String): Either[Errors, User] =
    (usernameCheckKleisli(username), emailCheckKleisli(email)).mapN(User)

  createUserKleisli("Eanea", "eanea@eanea.eanea").print
  createUserKleisli("", "dave@underscore.io@io").print

}
