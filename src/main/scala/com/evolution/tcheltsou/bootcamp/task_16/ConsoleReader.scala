package com.evolution.tcheltsou.bootcamp.task_16

import cats.effect.{Resource, Sync}
import cats.syntax.all._

import scala.io.Source

trait ConsoleReader[F[_]] {
  def readFromFile: F[String]
  def readSeed: F[Int]
}

object ConsoleReader {

  def of[F[_]: Sync](console: Console[F]): F[ConsoleReader[F]] =
    new ConsoleReader[F] {

      private def fromConsole[A](reader: String => A, name: String): F[A] =
        for {
          _      <- console.putStr("Blocker's thread: " + Main.getCurrentThreadName)
          _      <- console.putStr(s"Please input $name: ")
          input  <- console.readStr
          result <- Sync[F]
            .delay(reader(input))
            .handleErrorWith(e => console.putStr(e.getMessage + "\nPlease try again") >> fromConsole(reader, name))
        } yield result

      override def readFromFile: F[String] =
        Resource
          .fromAutoCloseable(fromConsole(Source.fromFile(_), "file path"))
          .use(src => Sync[F].delay(src.mkString))

      override def readSeed: F[Int] = fromConsole(_.toInt, "seed")

    }.pure
}
