package com.evolution.tcheltsou.bootcamp.task_15

import scala.concurrent.Future
import scala.util.Try

object EffectsHomework {
  final class IO[A](private val run: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(run()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(run()).run())
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = map(_ => ())
    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)
    def option: IO[Option[A]] = attempt.map(_.toOption)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = redeemWith(f, IO.pure)
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = attempt.map(_.fold(recover, map))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.flatMap(_.fold(recover, bind))
    def unsafeRunSync(): A = run()
    def unsafeToFuture(): Future[A] = ???
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = thunk
    def delay[A](body: => A): IO[A] = new IO(() => body)
    def pure[A](a: A): IO[A] = IO(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e.fold(raiseError, pure)
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = fromEither(option.toRight(orElse))
    def fromTry[A](t: Try[A]): IO[A] = fromEither(t.toEither)
    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = IO(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = whenA(!cond)(raiseError(e))
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = whenA(cond)(raiseError(e))
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = whenA(!cond)(action)
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    val unit: IO[Unit] = IO(())
  }
}
