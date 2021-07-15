package com.evolution.tcheltsou.bootcamp.task_16

import cats.effect.Sync
import cats.syntax.all._
import scala.io.StdIn

trait Console[F[_]] {
  def readStr: F[String]
  def putStr(text: String): F[Unit]
}

object Console {
  def apply[F[_]: Sync]: F[Console[F]] =
    new Console[F] {
      override def readStr: F[String]            = Sync[F].delay(StdIn.readLine())
      override def putStr(text: String): F[Unit] = Sync[F].delay(println(text))
    }.pure
}
