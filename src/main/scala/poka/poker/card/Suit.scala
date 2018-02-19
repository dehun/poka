package poka.poker.card

sealed trait Suit
case object Hearts extends Suit {
  override def toString: String = "♥"
}
case object Diamonds extends Suit {
  override def toString: String = "♦"
}
case object Spades extends Suit {
  override def toString: String = "♠"
}
case object Clubs extends Suit {
  override def toString: String = "♣"
}
