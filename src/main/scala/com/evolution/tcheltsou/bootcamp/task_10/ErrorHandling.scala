package com.evolution.tcheltsou.bootcamp.task_10

import cats.data.ValidatedNec

import java.time.YearMonth
import cats.syntax.all._

import scala.math.Ordered.orderingToOrdered

object ErrorHandling {

  private val HolderNameRegex = ???

  final case class CardNumber(number: Long) extends AnyVal
  final case class HolderName(name: String) extends AnyVal
  final case class SecurityCode(securityCode: Int) extends AnyVal
  final case class PaymentCard(
    name: HolderName,
    number: CardNumber,
    expirationDate: YearMonth,
    securityCode: SecurityCode
  )

  sealed trait ValidationError
  case object HolderNameIsNotValid extends ValidationError
  case object CardNumberParsError extends ValidationError
  case object CardNumberIsNotValid extends ValidationError
  case object ExpirationDateParseError extends ValidationError
  case object ExpirationDateIsExpired extends ValidationError
  case object SecurityCodeParseError extends ValidationError
  case object SecurityCodeIsNotValid extends ValidationError

  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]
    type ErrorOr[A] = Either[ValidationError, A]

    private def validateName(name: String): ErrorOr[HolderName] =
      Either.cond(name.matches(HolderNameRegex), HolderName(name), HolderNameIsNotValid)

    private def parse[A](value: String, parser: String => A, error: ValidationError): ErrorOr[A] =
      Either.catchNonFatal(parser(value)).leftMap(_ => error)

    private def validateCardNumber(number: String): ErrorOr[CardNumber] = {
      def isValidCardNumber(number: Long): Boolean = ???

      parse(number, _.toLong, CardNumberParsError)
        .ensure(CardNumberIsNotValid)(isValidCardNumber)
        .map(CardNumber)
    }

    private def validateExpirationDate(date: String): ErrorOr[YearMonth] =
      parse(date, YearMonth.parse, ExpirationDateParseError)
        .ensure(ExpirationDateIsExpired)(_ >= YearMonth.now())

    private def validateSecurityCode(code: String): ErrorOr[SecurityCode] =
      parse(code, _.toInt, SecurityCodeParseError)
        .ensure(SecurityCodeIsNotValid)((1 to 999).contains)
        .map(SecurityCode)

    def validate(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String,
    ): AllErrorsOr[PaymentCard] = (
      validateName(name).toValidatedNec,
      validateCardNumber(number).toValidatedNec,
      validateExpirationDate(expirationDate).toValidatedNec,
      validateSecurityCode(securityCode).toValidatedNec
    ).mapN(PaymentCard)
  }
}
