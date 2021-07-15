package com.evolution.tcheltsou.bootcamp.task_15

import scala.concurrent.Future
import scala.util.Try

class Fun {
  trait IO[+A] {
    def map[B](f: A => B): IO[B]          = Map(this, f)
    def flatMap[B](f: A => IO[B]): IO[B]  = FlatMap(this, f)
    def *>[B](another: IO[B]): IO[B]      = flatMap(_ => another)
    def as[B](newValue: => B): IO[B]      = map(_ => newValue)
    def void: IO[Unit]                    = map(_ => ())
    def attempt: IO[Either[Throwable, A]] =
      FlatMap(this, (a: A) => Pure(Right(a))).handleErrorWith(e => Pure(Left(e)))
    def option: IO[Option[A]] = attempt.map(_.toOption)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = HandleErrorWith(this, f)
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] =
      redeemWith(recover.andThen(Pure(_)), map.andThen(Pure(_)))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
      FlatMap(this, bind).handleErrorWith(recover)
    def unsafeRunSync(): A = {
      def loop(current: IO[Any], stack: List[Bind]): A = current match {
//        case Map(io, f)             => loop(io, Bind.F(f.andThen(Pure(_))) :: stack)
//        case FlatMap(io, f)         => loop(io, Bind.F(f) :: stack)
        case HandleErrorWith(io, h) => loop(io, Bind.H(h) :: stack)
        case Delay(body)            => Try(body).fold(e => loop(RaiseError(e), stack), a => loop(Pure(a), stack))
        case Pure(a) =>
          stack.dropWhile(_.isHandler) match {
            case Nil               => a.asInstanceOf[A]
            case Bind.F(f) :: tail => loop(f(a), tail)
          }
        case RaiseError(e) =>
          stack.dropWhile(!_.isHandler) match {
            case Nil               => throw e
            case Bind.H(h) :: tail => loop(h(e), tail)
          }
      }
      loop(this, Nil)
    }
    def unsafeToFuture(): Future[A] = ???
  }
  final case class Pure[+A](a: A) extends IO[A]
  final case class Map[B, +A](io: IO[B], f: B => A) extends IO[A]
  final case class FlatMap[B, +A](io: IO[B], f: B => IO[A]) extends IO[A]
  final case class Delay[+A](eff: () => A) extends IO[A]
  final case class RaiseError(e: Throwable) extends IO[Nothing]
  final case class HandleErrorWith[+A](io: IO[A], f: Throwable => IO[A]) extends IO[A]

  trait Bind {
    def isHandler: Boolean = isInstanceOf[Bind.H]
  }
  object Bind {
    final case class F(f: Any => IO[Any]) extends Bind
    final case class H(h: Throwable => IO[Any]) extends Bind
  }

  object IO {
    def apply[A](body: => A): IO[A]                                   = Delay(() => body)
    def suspend[A](thunk: => IO[A]): IO[A]                            = unit *> thunk
    def delay[A](body: => A): IO[A]                                   = apply(body)
    def pure[A](a: A): IO[A]                                          = Pure(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A]                 = e.fold(raiseError, pure)
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = fromEither(option.toRight(orElse))
    def fromTry[A](t: Try[A]): IO[A]                                  = fromEither(t.toEither)
    def none[A]: IO[Option[A]]                                        = pure(None)
    def raiseError[A](e: Throwable): IO[A]                            = RaiseError(e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit]         = whenA(!cond)(raiseError(e))
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit]           = whenA(cond)(raiseError(e))
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit]         = whenA(!cond)(action)
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit]           = if (cond) action else unit
    val unit: IO[Unit]                                                = Pure(())
  }

}
