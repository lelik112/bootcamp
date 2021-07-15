package com.evolution.tcheltsou.bootcamp.task_1

object Basics {

  def gcd(a: Int, b: Int): Option[Int]  = gcdRecursion(a.abs, b.abs)
  def lcm(a: Int, b: Int): Option[Long] =
    gcd(a, b)
      .map(g => (a.toLong * b / g).abs)
      .filterNot(_ == 0L)

  //https://en.wikipedia.org/wiki/Binary_GCD_algorithm
  private def gcdRecursion(a: Int, b: Int): Option[Int] = (a, b) match {
    case (0, 0)           => None
    case (m, 0)           => Some(m)
    case (0, n)           => Some(n)
    case (m, n) if m == n => Some(m)
    case (m, n)           => (isEven(m), isEven(n)) match {

      case (true , true ) => gcdRecursion(m >>> 1, n >>> 1).map(_ << 1)
      case (true , false) => gcdRecursion(m >>> 1, n      )
      case (false, true ) => gcdRecursion(m      , n >>> 1)
      case _ if m > n     => gcdRecursion((m - n) >>> 1, n)
      case _              => gcdRecursion((n - m) >>> 1, m)
    }
  }

  private def isEven(n: Int): Boolean = (n & 1) == 0
}
