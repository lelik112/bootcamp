package com.evolution.tcheltsou.bootcamp.task_16

import cats.Parallel
import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, Sync}
import cats.implicits._

object Exerciser {

  def sign[F[_]: ContextShift: Sync: Parallel](reader: ConsoleReader[F]): F[Ref[F, Option[Int]]] =
    for {
      data <- reader.readFromFile
      seed <- reader.readSeed
      sign <- Signer.sign[F](data, seed)
      ref  <- Ref.of[F, Option[Int]](sign)
    } yield ref
}
