object FunctionalEffectsExample extends App {

  //This immutable value doesn't do anything—it just describes a program that prints out a message, asks for input, and prints out another message that depends on the input.
  val example1: Console[Unit] =
    PrintLine("Hello, what is your name?",
              ReadLine(name =>
                PrintLine(s"Good to meet you, $name", Return(() => ()))))

  /*
    Create an interpreter for our program. This is a Procedural Scala programs and not a functional scala program.
    Interpreting (also called running or executing) is not functional, because it may be partial, non-deterministic, and impure.
    In an ideal application, however,
    interpretation only needs to happen once: in the application's main function. The rest of the application can be purely functional.
   */
  @scala.annotation.tailrec
  def interpreter[A](console: Console[A]): A = console match {
    case Return(value) => value()
    case PrintLine(line, next) =>
      println(line)
      interpreter(next)
    case ReadLine(next) =>
      interpreter(next(io.StdIn.readLine()))
  }

  import HelperFunctions._

  val example2: Console[Unit] = for {
    _    <- printLine("Hello, what is your name?")
    name <- readLine
    _    <- printLine(s"Good to meet you, $name")
  } yield ()

  interpreter(example1)
  interpreter(example2)
}

/*
In practice, it's not very convenient to build console programs using constructors directly.
Instead, we can define helper functions, which look more like their effectful equivalents:
 */
object HelperFunctions {
  def succeed[A](value: => A): Console[A] = Return(() => value)
  def printLine[A](line: String): Console[Unit] =
    PrintLine(line, succeed())
  def readLine[A]: Console[String] = ReadLine(line => succeed(line))

  implicit class ConsoleSyntax[+A](console: Console[A]) {
    def map[B](f: A => B): Console[B] =
      flatMap(a => succeed(f(a)))

    def flatMap[B](f: A => Console[B]): Console[B] = {
      console match {
        case Return(value)         => f(value())
        case PrintLine(line, next) => PrintLine(line, next.flatMap(f))
        case ReadLine(next)        => ReadLine(line => next(line).flatMap(f))
      }
    }
  }
}

//data structure to describe a console program
sealed trait Console[+A]
final case class Return[A](value: () => A) extends Console[A]
final case class PrintLine[A](line: String, rest: Console[A])
    extends Console[A]
final case class ReadLine[A](rest: String => Console[A]) extends Console[A]
