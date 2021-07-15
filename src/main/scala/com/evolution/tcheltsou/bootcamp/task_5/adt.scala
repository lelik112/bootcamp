package com.evolution.tcheltsou.bootcamp.task_5

//https://github.com/lelik112/poker ('evo-home-task' user has access to the project)
object adt {
  type Rank = Int
  val Ranks: Range = 2 to 14

  sealed trait Suit
  case object Spades   extends Suit
  case object Hearts   extends Suit
  case object Diamonds extends Suit
  case object Clubs    extends Suit

  sealed abstract case class Card private(suit: Suit, rank: Rank)
  object Card {
    def apply(suit: Suit, rank: Rank): Option[Card] =
      if (Ranks.contains(rank)) Some(new Card(suit, rank){}) else None
  }

  sealed case class Cards(cards: Set[Card]) {
    def toHand: Option[Hand] = Hand(cards)
  }

  sealed abstract class Hand private(override val cards: Set[Card]) extends Cards(cards)
  object Hand {
    def apply(cards: Set[Card]): Option[Hand] =
      if (cards.size == 5) Some(new Hand(cards){}) else None
  }

  type Finder = Cards => Option[Cards]
}

