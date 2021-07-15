package com.evolution.tcheltsou.bootcamp.task_4

object Collections {

  // try to implement min different ways (fold, reduce, recursion)
  def min(list: List[Int]): Option[Int] =
    list.reduceOption(_ min _)

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] =
    list.foldLeft(zero :: Nil) { (l, v) =>
      f(l.head, v) :: l
    }.reverse

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  def count(s: String): List[(Char, Int)] = {
    s.foldRight[List[(Char, Int)]](Nil) {
      case (next, (current, n) :: xs) if current == next => (next, n + 1) :: xs
      case (next, result            )                    => (next, 1) :: result
    }
  }

  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(num: Array[Int]): Array[Int] =
    num.scanLeft(0)(_ + _).tail

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(num: Array[Int]): Array[Int] = {
    val n = num.length / 2

    (0 until n)
      .flatMap(i => List(num(i), num(i + n)))
      .toArray
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int =
    accounts.map(_.sum).max

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val max = candies.max
    candies.map(_ + extraCandies >= max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[(Int, Int)]): Int = {
    val temp = points.map(_._1).sorted
    temp.zip(temp.tail).map(p => p._2 - p._1).max
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(string: String): Int =
    string.foldLeft(0, 0) {
      case ((c, v), '(') => (c max (v + 1), v + 1)
      case ((c, v), ')') => (c, v - 1)
      case (z     ,  _ ) => z
    }._1

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(string: String): Int =
    string.foldLeft(0, 0) {
      case ((c, 0), 'R') => (c + 1, 1)
      case ((c, 0), 'L') => (c + 1, -1)
      case ((c, b), 'R') => (c, b + 1)
      case ((c, b), 'L') => (c, b - 1)
    }._1

  // https://leetcode.com/problems/matrix-block-sum/
  def matrixBlockSum(matrix: Array[Array[Int]], k: Int): Array[Array[Int]] = {
    def sum(i: Int, j: Int): Int =
      (for {
        v <- (i - k) to (i + k) if matrix.isDefinedAt(v)
        h <- (j - k) to (j + k) if matrix(v).isDefinedAt(h)
      } yield matrix(v)(h)).sum

    val result = Array.ofDim[Int](matrix.length, matrix.map(_.length).maxOption.getOrElse(0))

    for {
      i <- matrix.indices
      j <- matrix(i).indices
    } result(i)(j) = sum(i, j)

    result
  }
}
