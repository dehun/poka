package poka.poker


import poka.poker.card._
import cats.data._
import cats.implicits._
import cats._


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
  def poll:Option[(Deck, Card)] = cards match {
    case x::xs => Some(Deck(xs), x)
    case _ => None
  }

  def pollN(n:Int):Option[(Deck, List[Card])] = {
    type DeckOptState[A] = StateT[Option, Deck, A]
    (1 to n).toList.traverse[DeckOptState, Card] {_:Int =>
      for {
        deck <- StateT.get[Option, Deck]
        cd <- StateT.liftF[Option, Deck, (Deck, Card)](deck.poll)
        _ <- StateT.set[Option, Deck](cd._1)
      } yield cd._2
    }.run(this)
  }
}
