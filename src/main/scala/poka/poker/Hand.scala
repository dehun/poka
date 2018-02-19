package poka.poker

import scala.math.Ordering
import poka.poker.card.{Card, King, Numbered}

case class Hand(inHand:Set[Card], onDesk:Set[Card]) {
  lazy val allCards:Set[Card] = inHand ++ onDesk
}

object Hand {
  private case class CombinationRank(rank:Int)

  private implicit val combinationRankOrdering = new Ordering[CombinationRank] {
    override def compare(x: CombinationRank, y: CombinationRank): Int = {
      Ordering[Int].compare(x.rank, y.rank)
    }
  }

  private implicit val highestCardRankOrdering = new Ordering[HighestCardRank] {
    override def compare(x: HighestCardRank, y: HighestCardRank): Int = {
      val lhs = x.set.toList.sorted.reverse
      val rhs = y.set.toList.sorted.reverse
      lhs.zip(rhs).find({case (lc, rc) => Ordering[Card].compare(lc, rc) != 0}) match {
        case None => 0
        case Some((lc, rc)) => Ordering[Card].compare(lc, rc)
      }
    }
  }

  private case class HighestCardRank(set:Set[Card])

  implicit val handsOrder = new Ordering[Hand] {
    import poka.poker.card.{Ace}
    private def isRoyalFlush(hand: Hand):Boolean = isStraightFlush(hand) && hand.allCards.map(_.rank).contains(Ace)
    private def isStraightFlush(hand:Hand):Boolean = isStraight(hand) && isFlush(hand)
    private def isFlush(hand: Hand):Boolean = hand.allCards.forall(c => c.suit == hand.allCards.head.suit)
    private def isStraight(hand:Hand):Boolean = {
      val s = hand.allCards.toList.filter(_.rank != Ace).sorted
      if (hand.allCards.exists(_.rank == Ace)) {
        if (hand.allCards.count(_.rank == Ace) > 1) false
        else {
          s.max.rank == King || s.min.rank == Numbered(1)
        }
      } else {
        s.init.zip(s.tail).forall { case (l, r) => r.rank.toNum - l.rank.toNum == 1 }
      }
    }

    private def isFourOfAKind(hand:Hand):Boolean = {
      hand.allCards.groupBy(_.rank).map(_._2.size).toSet == Set(4, 1)
    }

    private def isFullHouse(hand:Hand):Boolean = {
      hand.allCards.groupBy(_.rank).map(_._2.size).toSet == Set(2, 3)
    }

    private def isTwoPair(hand:Hand):Boolean = {
      hand.allCards.groupBy(_.rank).map(_._2.size).toSet == Set(2, 2, 1)
    }

    private def isThreeOfAKind(hand:Hand):Boolean = {
      hand.allCards.groupBy(_.rank).map(_._2.size).exists(_ == 3)
    }

    private def isPair(hand:Hand):Boolean = {
      hand.allCards.groupBy(_.rank).map(_._2.size).exists(_ == 2)
    }

    private def isHighestCard(hand:Hand):Boolean = true

    private val combinations:Vector[(Hand => Boolean)] = Vector(
      isHighestCard,
      isPair,
      isTwoPair,
      isThreeOfAKind,
      isStraight,
      isFlush,
      isFullHouse,
      isFourOfAKind,
      isStraightFlush,
      isRoyalFlush
    )

    private def combinationRank(h:Hand) = combinations.zipWithIndex.find({case (p, _) => p(h)}) match {
      case None => CombinationRank(0)
      case Some((_, idx)) => CombinationRank(1 + idx)
    }

    override def compare(lhs: Hand, rhs: Hand): Int = {
      implicitly[Ordering[(CombinationRank, HighestCardRank)]].compare(
        (combinationRank(lhs), HighestCardRank(lhs.allCards)),
        (combinationRank(rhs), HighestCardRank(rhs.allCards)))
    }
  }
}