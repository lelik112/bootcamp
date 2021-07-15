package com.evolution.tcheltsou.bootcamp.task_3

import scala.io.Source
import scala.util.Try

object ControlStructures {

  val BlankRegex        : String = "\\s+"

  val DivideCommandName : String = "divide"
  val SumCommandName    : String = "sum"
  val AverageCommandName: String = "average"
  val MinCommandName    : String = "min"
  val MaxCommandName    : String = "max"

  val DivideResultName : String = "divided"
  val SumResultName    : String = "sum"
  val AverageResultName: String = "average"
  val MinResultName    : String = "minimum"
  val MaxResultName    : String = "maximum"

  sealed trait Command

  final case class Divide(dividend: Double, divisor: Double) extends Command
  final case class Sum(numbers: List[Double]) extends Command
  final case class Average(numbers: List[Double]) extends Command
  final case class Min(numbers: List[Double]) extends Command
  final case class Max(numbers: List[Double]) extends Command

  final case class ErrorMessage(value: String)

  object ErrorMessage {
    def apply(massage: String, input: String): ErrorMessage =
      ErrorMessage(s"Error: $massage. Input: $input")
  }

  final case class Result(name: String, of: List[Double], is: Double)

  def parseCommand(input: String): Either[ErrorMessage, Command] = {
    val commandName :: values = input.toLowerCase.split(BlankRegex).toList

    Try(values.map(_.toDouble)).toEither
      .left.map(_ => "Can not parse values")
      .flatMap { numbers =>
               (commandName       , numbers        ) match {
          case (_                 , Nil            ) => Left("Input has to have at least one number")
          case (DivideCommandName , _ :: 0.0 :: Nil) => Left("Division by zero")
          case (DivideCommandName , x :: y   :: Nil) => Right(Divide(x, y))
          case (DivideCommandName , _              ) => Left("Divide input has to have exactly two numbers")
          case (SumCommandName    , _              ) => Right(Sum(numbers))
          case (AverageCommandName, _              ) => Right(Average(numbers))
          case (MinCommandName    , _              ) => Right(Min(numbers))
          case (MaxCommandName    , _              ) => Right(Max(numbers))
          case _                                     => Left("Unknown command")
        }
      }
      .left.map(ErrorMessage(_, input))
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = {
    command match {
      case Divide(_, 0.0)   => Left(ErrorMessage(s"Error: Division by zero: $command"))
      case Divide(x, y)     => Right(Result(DivideResultName, List(x, y), x / y))
      case Sum(numbers)     => Right(Result(SumResultName, numbers, numbers.sum))
      case Average(numbers) => Right(Result(AverageResultName, numbers, numbers.sum / numbers.size))
      case Min(numbers)     => Right(Result(MinResultName, numbers, numbers.min))
      case Max(numbers)     => Right(Result(MaxResultName, numbers, numbers.max))
    }
  }

  def renderResult(result: Result): String = {
    if (result.name == DivideResultName)
      s"${result.of.head} ${result.name} by ${result.of.last} is ${result.is}"
    else
      s"the ${result.name} of ${result.of.mkString(" ")} is ${result.is}"
  }

  def process(input: String): String = {
    (for {
      command <- parseCommand(input)
      result  <- calculate(command)
    } yield renderResult(result)).left.map(_.value).merge
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
