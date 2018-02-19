package poka.poker

import poka.poker.card._

import scala.util.Random

object Deck {
  val orderedDeck = Deck(for {
    suit <- List(Hearts, Diamonds, Spades, Clubs)
    rank <- Ace :: King :: Queen :: Jack :: (2 to 10).map(Numbered).toList
  } yield Card(suit, rank))

  def nextRandom(r:Random):Deck = {
    Deck(r.shuffle(orderedDeck.cards))
  }
}

case class Deck(cards:List[Card]) {
  def poll:Option[(Card, Deck)] = cards match {
    case x::xs => Some(x, Deck(xs))
    case _ => None
  }
}
