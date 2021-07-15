package com.evolution.tcheltsou.bootcamp.task_7

object Task0 {

  trait HashCode[A] {
    def hash(value: A): Int
  }

  object HashCode {
    def apply[A: HashCode]: HashCode[A] = implicitly[HashCode[A]]
  }

  implicit class HashCodeOps[A: HashCode](val x: A) {
    def hash: Int = HashCode[A].hash(x)
  }

  implicit val stringHashCode: HashCode[String] = _.hashCode
}

object Task1 {

  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 {

  trait Show[T] {
    def show(entity: T): String
  }

  object Show {
    def apply[A: Show]: Show[A] = implicitly[Show[A]]
  }

  final case class User(id: String, name: String)

  implicit val userShow: Show[User] = _.toString + "(fancy one)"

  implicit class ShowOps[A: Show](val value: A) {
    def show: String = Show[A].show(value)
  }
}

object Task3 {
  type Error = String

  trait Parse[T] {
    def parse(entity: String): Either[Error, T]
  }

  object Parse {
    def apply[A: Parse]: Parse[A] = implicitly[Parse[A]]
  }

  final case class User(id: String, name: String)

  implicit val userParse: Parse[User] = _.split(",") match {
    case Array(id, name) => Right(User(id, name))
    case _ => Left(s"Can not parse input")
  }

  implicit class ParseOps(val input: String) {
    def parse[T: Parse]: Either[Error, T] = Parse[T].parse(input)
  }
}

object Task4 {

  trait Equal[A] {
    def areEqual(left: A, right: A): Boolean
  }

  object Equal {
    def apply[A: Equal]: Equal[A] = implicitly[Equal[A]]
  }

  implicit class EqualOps[A: Equal](val left: A) {
    def ===(right: A): Boolean = Equal[A].areEqual(left, right)
  }
}

object AdvancedHomework {

  trait Monad[F[_]] {
    def id[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => id(f(a)))
  }

  object Monad {
    def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]
  }

  implicit class MonadOps[F[_]: Monad, A](val monad: F[A]) {
    def id(a: A): F[A] = Monad[F].id(a)
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(monad)(f)
    def map[B](f: A => B): F[B] = Monad[F].map(monad)(f)
  }

  sealed trait Maybe[+A]
  final case class Just[A](value: A) extends Maybe[A]
  case object None extends Maybe[Nothing]

  implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {
    override def id[A](a: A): Maybe[A] = Just(a)
    override def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] = fa match {
      case Just(a) => f(a)
      case None    => None
    }
  }
}
