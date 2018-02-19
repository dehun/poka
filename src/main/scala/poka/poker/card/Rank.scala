package poka.poker.card

sealed trait Rank {
  def toNum:Int
}

object Rank {
  implicit val rankOrdering = new Ordering[Rank] {
    override def compare(x: Rank, y: Rank): Int = {
      implicitly[Ordering[Int]].compare(x.toNum, y.toNum)
    }
  }
}

case class Numbered(rank:Int) extends Rank {
  val toNum = rank
  require(rank >= 2)
  require(rank <= 10)

  override def toString: String = rank.toString
}

case object Jack extends Rank {
  val toNum = 11
  override def toString: String = "J"
}

case object Queen extends Rank {
  val toNum = 12
  override def toString: String = "Q"
}

case object King extends Rank {
  val toNum = 13
  override def toString: String = "K"
}

case object Ace extends Rank {
  val toNum = 14
  override def toString: String = "A"
}


