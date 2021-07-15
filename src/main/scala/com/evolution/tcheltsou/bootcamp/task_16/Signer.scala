package com.evolution.tcheltsou.bootcamp.task_16

import cats.Parallel
import cats.effect.Sync
import cats.implicits._

object Signer {

  private def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }
  private def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }

  def sign[F[_]: Sync: Parallel](content: String, seed: Int): F[Option[Int]] = {
    val words = content.split("\\s").toVector
    val signers: List[(String, Int) => Int] = List(javaHash, knuthHash)
    signers.parTraverse { f =>
      words.parTraverse(w => Sync[F].pure(f(w, seed))).map(_.minOption)
    }.map(_.sequence.map(_.min))
  }

}
