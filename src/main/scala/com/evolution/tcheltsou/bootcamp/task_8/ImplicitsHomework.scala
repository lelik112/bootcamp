package com.evolution.tcheltsou.bootcamp.task_8

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {

  private val HeaderScore = 12

  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object GetSizeScore {
      def apply[T: GetSizeScore]: GetSizeScore[T] = implicitly[GetSizeScore[T]]
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }
    }

    /** Mutable key-value cache which limits the size score of the data scored.
      *
      * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
      * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
      * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
      * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
      *
      * @param maxSizeScore max size score for the stored data
      * @tparam K key type
      * @tparam V value type
      */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      import syntax._

      require(maxSizeScore >= 0)

      private val map    = mutable.LinkedHashMap.empty[K, V]
      //non-thread-safe + one-more-var = non-thread-safe VS O(n)
      private var scores = 0

      def put(key: K, value: V): Unit = {
        reduceScores(key)
        map += key -> value
        scores += (key.sizeScore + value.sizeScore)
        checkScores()
      }

      def get(key: K): Option[V] = map.get(key)

      private def reduceScores(key: K): Unit =
        map.get(key).map(_.sizeScore + key.sizeScore).foreach(scores -= _)

      @tailrec
      private def checkScores(): Unit =
        if (scores > maxSizeScore) {
          //IllegalStateException VS illegal state
          require(map.nonEmpty)
          val (key, _) = map.head
          reduceScores(key)
          map -= key
          checkScores()
        }
    }

    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V]                  = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }
    object Iterate {
      def apply[F[_]: Iterate]: Iterate[F] = implicitly[Iterate[F]]
    }

    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }
    object Iterate2 {
      def apply[F[_, _]: Iterate2]: Iterate2[F] = implicitly[Iterate2[F]]
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }
      implicit val packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.iterator.map { case (k, _) => k }
        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.iterator.map { case (_, v) => v }
      }

      implicit val byteGetSizeScore: GetSizeScore[Byte] = _ => 1
      implicit val charGetSizeScore: GetSizeScore[Char] = _ => 2
      implicit val intGetSizeScore: GetSizeScore[Int]   = _ => 4
      implicit val longGetSizeScore: GetSizeScore[Long] = _ => 8
      implicit val stringGetSizeScore: GetSizeScore[String] =
        value => HeaderScore + value.length * 2

      implicit def iterateGetSizeScore[F[_]: Iterate, T: GetSizeScore]: GetSizeScore[F[T]] =
        values => HeaderScore + scores(Iterate[F].iterator(values))

      implicit def iterate2GetSizeScore[F[_, _]: Iterate2, T: GetSizeScore, S: GetSizeScore]: GetSizeScore[F[T, S]] =
        values => HeaderScore + scores(Iterate2[F].iterator1(values)) + scores(Iterate2[F].iterator2(values))

      private def scores[A: GetSizeScore](iterator: Iterator[A]): Int =
        iterator.map(_.sizeScore).sum
    }
  }

  object MyTwitter {
    import SuperVipCollections4s._
    import SuperVipCollections4s.syntax._
    import SuperVipCollections4s.instances._

    final case class Twit(
      id: Long,
      userId: Int,
      hashTags: Vector[String],
      attributes: PackedMultiMap[String, String],
      fbiNotes: List[FbiNote],
    )

    final case class FbiNote(
      month: String,
      favouriteChar: Char,
      watchedPewDiePieTimes: Long,
    )

    implicit val FbiNoteGetSizeScore: GetSizeScore[FbiNote] =
      v => HeaderScore + v.month.sizeScore + v.favouriteChar.sizeScore + v.watchedPewDiePieTimes.sizeScore
    implicit val TwitGetSizeScore: GetSizeScore[Twit] =
      t =>
        HeaderScore + t.id.sizeScore + t.userId.sizeScore + t.hashTags.sizeScore + t.attributes.sizeScore + t.fbiNotes.sizeScore

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      val cash = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit       = cash.put(twit.id, twit)
      override def get(id: Long): Option[Twit] = cash.get(id)
    }
  }
}
