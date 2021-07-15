package com.evolution.tcheltsou.bootcamp.task_17

import cats.Monad
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Resource, Timer}
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration

object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]
    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_]: Clock: Monad, K, V](
    state: Ref[F, Map[K, (Long, V)]],
    expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {

    private def realTime = Clock[F].realTime(MILLISECONDS)
    private def expiresAt(currentTime: Long) = currentTime + expiresIn.toMillis
    private def updateMap(key: K, value: V, time: Long)(map: Map[K, (Long, V)]) = map + (key -> (time, value))

    def get(key: K): F[Option[V]] = {
      def refreshWithTime(time: Long)(currentState: Map[K, (Long, V)]): Map[K, (Long, V)] =
        currentState.get(key).fold(currentState) {
          case (_, value) => updateMap(key, value, time)(currentState)
        }

      for {
        currentTime <- realTime
        values      <- state.getAndUpdate(refreshWithTime(expiresAt(currentTime)))
      } yield values.get(key).map { case (_, value) => value }
    }

    def put(key: K, value: V): F[Unit] =
      realTime.flatMap(currentTime => state.update(updateMap(key, value, expiresAt(currentTime))))

  }

  object Cache {
    def of[F[_]: Clock, K, V](
      expiresIn: FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): Resource[F, Cache[F, K, V]] = {

      def cleanUp(state: Ref[F, Map[K, (Long, V)]]): F[Unit] =
        (for {
          _           <- T.sleep(checkOnExpirationsEvery)
          currentTime <- Clock[F].realTime(MILLISECONDS)
          _           <- state.update(_.filter { case (_, (time, _)) => currentTime < time })
        } yield ()).foreverM

      for {
        state <- Resource.eval(Ref.of[F, Map[K, (Long, V)]](Map.empty))
        _     <- C.background(cleanUp(state))
      } yield new RefCache(state, expiresIn)
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {
    Cache.of[IO, Int, String](10.seconds, 4.seconds).use { cache =>
      for {
        _ <- cache.put(1, "Hello")
        _ <- cache.put(2, "World")
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
        _ <- IO.sleep(6.seconds)
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- IO.sleep(6.seconds)
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
      } yield ExitCode.Success
    }
  }
}
