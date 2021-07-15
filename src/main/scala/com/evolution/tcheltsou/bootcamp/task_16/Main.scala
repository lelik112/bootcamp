package com.evolution.tcheltsou.bootcamp.task_16

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}

object Main extends IOApp {
  def getCurrentThreadName: String = Thread.currentThread().getName

  override def run(args: List[String]): IO[ExitCode] = {
    val resultResource = for {
      blocker <- Blocker[IO]
      console <- Resource.liftF(Console[IO])
      reader  <- Resource.liftF(ConsoleReader.of[IO](console))
      _       <- Resource.liftF(console.putStr("Main thread: " + getCurrentThreadName))
      _       <- Resource.liftF(IO.contextShift(blocker.blockingContext).shift)
      _       <- Resource.liftF(console.putStr("Blocker's thread: " + getCurrentThreadName))
      ref     <- Resource.liftF(Exerciser.sign(reader))
      _       <- Resource.liftF(console.putStr("Main thread (but WHY???): " + getCurrentThreadName))
      sign    <- Resource.liftF(ref.get)
    } yield sign

    resultResource.use(sign => IO(println(s"Got it in main thread $getCurrentThreadName: $sign"))).as[ExitCode](ExitCode.Success)
  }
}
