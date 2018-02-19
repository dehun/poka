package poka.poker.card

case class Card(suit: Suit, rank: Rank) {
  override def toString: String = { s"<$suit:$rank>" }
}

object Card {
  implicit val cardsOrder = new Ordering[Card] {
    import poka.poker.card.Rank._
    override def compare(x: Card, y: Card): Int = implicitly[Ordering[Rank]].compare(x.rank, y.rank)
  }
}
