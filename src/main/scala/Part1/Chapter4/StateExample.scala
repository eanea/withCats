package Part1.Chapter4

import cats.data.State

import Print.PrintSyntax._

object StateExample extends App {
  val getDemo = State.get[Int]
  // getDemo: State[Int, Int] = cats.data.IndexedStateT@796af713
  getDemo.run(10).value.print
  // res1: (Int, Int) = (10, 10)
  val setDemo = State.set[Int](30)
  // setDemo: State[Int, Unit] = cats.data.IndexedStateT@f9e66fa
  setDemo.run(10).value.print
  // res2: (Int, Unit) = (30, ())
  val pureDemo = State.pure[Int, String]("Result")
  // pureDemo: State[Int, String] = cats.data.IndexedStateT@439e3ee4
  pureDemo.run(10).value.print
  // res3: (Int, String) = (10, "Result")
  val inspectDemo = State.inspect[Int, String](x => s"${x}!")
  // inspectDemo: State[Int, String] = cats.data.IndexedStateT@77263be4
  inspectDemo.run(10).value.print
  // res4: (Int, String) = (10, "10!")
  val modifyDemo = State.modify[Int](_ + 1)
  // modifyDemo: State[Int, Unit] = cats.data.IndexedStateT@44ddcbfc
  modifyDemo.run(10).value.print
  // res5: (Int, Unit) = (11, ())

  type CalcState[A] = State[List[Int], A]
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" =>
//      "+".print
      calc(_ + _)
    case "-" =>
//      "-".print
      calc(_ - _)
    case "/" =>
//      "/".print
      calc(_ / _)
    case "*" =>
//      "*".print
      calc(_ * _)
    case num =>
//      num.print
      push(num.toInt)
  }

  def push(num: Int): CalcState[Int] = State { s => (num :: s, num) }

  def calc(f: (Int, Int) => Int): CalcState[Int] = State {
    case o1 :: o2 :: tail => (f(o1, o2) :: tail, f(o1, o2))
    case _                => throw new RuntimeException("unexpected state")
  }

//  evalOne("42").runA(Nil).value.print

  val program = for {
    _   <- evalOne("1")
    _   <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
//  program.runA(Nil).value.print
//   res11: Int = 3

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.pure[List[Int], Int](0))((s, sym) => s.flatMap(_ => evalOne(sym)))

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
//  multistageProgram.runA(Nil).value.print

  val biggerProgram = for {
    _   <- evalAll(List("1", "2", "+"))
    _   <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  biggerProgram.runA(Nil).value.print
  // res14: Int = 21

  def evalInput(input: String): Int = evalAll(input.split(" ").toList).runA(Nil).value

  evalInput("1 2 + 3 *").print

}
