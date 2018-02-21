package poka.poker

import poka.poker.Room.RoomError.{WrongMoveOrder, WrongTurnOrder}
import poka.poker.Room.RoomRound.PreFlop
import poka.poker.Room._
import poka.poker.card.Card

object Room {
  sealed trait PlayerMove
  object PlayerMove {
    case object Fold extends PlayerMove
    case object Check extends PlayerMove
    case class Call(amount:Int) extends PlayerMove
  }

  def empty(deck:Deck) = Room(Map.empty, Set.empty, PreFlop, deck, Option.empty)

  sealed trait RoomRound
  object RoomRound {
    case object PreFlop extends RoomRound
    case class Flop(cards:(Card, Card, Card)) extends RoomRound
    case class Turn(card:Card, flop: Flop) extends RoomRound
    case class River(card:Card, turn: Turn) extends RoomRound
    case class Ended(result:Map[PlayerId, Int], river: River) extends RoomRound
  }

  sealed trait RoomError
  object RoomError {
    case object WaitingForMorePlayers extends RoomError
    case object WrongTurnOrder extends RoomError
    case object WrongMoveOrder extends RoomError
    case object InternalGameLogicError extends RoomError
    case object TooManyPlayers extends RoomError
    case object InsufficientFunds extends RoomError
  }

  case class Stack(value:Int)
  case class PlayerState(putInAPot:Int, stackLeft:Stack, placeOnTable:Int, cards:Set[Card]) {
    def call(amount: Int): Either[RoomError.InsufficientFunds.type , PlayerState] = Right(this.copy(putInAPot))

    val isAllIn:Boolean = stackLeft.value == 0
  }
}

case class PlayerId(id:String)

case class Room(players:Map[PlayerId, PlayerState], folded:Set[PlayerId], roomRound:RoomRound, deck:Deck, nextMover:Option[PlayerId]) {
  def joinPlayer(playerId:PlayerId, stack:Stack):Room = {
    val newPlayer = (playerId -> PlayerState(0, stack, this.players.size, Set.empty))
    if (roomRound == PreFlop) {
      this.copy(players = this.players + newPlayer)
    } else {
      this.copy(players=this.players + newPlayer, folded=this.folded+playerId)
    }
  }

  private def findNextMoverOnTable(currentMover:PlayerId):Option[PlayerId] = {
    for {
      c <- players.get(currentMover)
      nm <- players.find {case (pid, ps) => !folded.contains(pid) && ps.placeOnTable > c.placeOnTable}
    } yield nm._1
  }

  private lazy val allInPlayers = players.filter({case (pid, ps) => ps.isAllIn}).keySet -- folded
  private lazy val inGamePlayers = players.keySet -- folded -- allInPlayers
  private lazy val biggestBet = players.map({case (pid, ps) => ps.putInAPot}).max
  private lazy val isAllBetsMatched = inGamePlayers.exists {pid => players(pid).putInAPot != biggestBet}

  import Room._
  def moveToNextRound(mover:PlayerId):Room = roomRound match {
    case RoomRound.PreFlop => {
      for {
        (newDeck, f :: s :: t :: Nil) <- deck.pollN(3)
        (newnewDeck, cardsForPlayers) <- deck.pollN(inGamePlayers.size)
        playersWithCards = inGamePlayers.zip(cardsForPlayers.grouped(2).toStream).foldLeft(this.players) {
          case (acc, (pid, cards)) => acc.updated(pid, acc(pid).copy(cards = cards.toSet))
        }
      } yield this.copy(roomRound = RoomRound.Flop((f, s, t)), players=playersWithCards, deck = newnewDeck)
    }.get // TODO: either
    case flop:RoomRound.Flop =>
      val (newDeck, card) = deck.poll.get
      this.copy(roomRound=RoomRound.Turn(card, flop), deck=newDeck)
    case turn:RoomRound.Turn =>
      val (newDeck, card) = deck.poll.get
      this.copy(roomRound=RoomRound.River(card, turn), deck=newDeck)
    case river:RoomRound.River =>
      // TODO: calculate winner
      this.copy(roomRound=RoomRound.Ended(Map.empty, river))
    case _:RoomRound.Ended => this
  }

  def playerMove(playerId:PlayerId, move:PlayerMove):Either[RoomError, Room] = {
    require(players.size >= 2) // TODO: replace with guard
    require((nextMover.isDefined && players.contains(nextMover.get) && folded.contains(nextMover.get)) || roomRound == PreFlop) // TODO: replace with guard
    require(!folded.contains(playerId)) // TODO: to guard

    val mover = nextMover.getOrElse(players.minBy(_._2.placeOnTable)._1)
    if (playerId != mover) Left(WrongTurnOrder)
    else {
      move match {
        // one can always fold
        case PlayerMove.Fold =>
          findNextMoverOnTable(mover) match {
            case newMover@Some(_) => Right(this.copy(folded=this.folded + playerId, nextMover=newMover))
            case None if isAllBetsMatched=> Right(moveToNextRound(mover))
            case None => Left(RoomError.WrongMoveOrder)
          }
        case PlayerMove.Check =>
          findNextMoverOnTable(mover) match {
            case newMover@Some(_) => Right(this.copy(nextMover=newMover))
            case None if isAllBetsMatched => Right(moveToNextRound(mover))
            case None => Left(RoomError.WrongMoveOrder)
          }
        case PlayerMove.Call(amount) =>
          this.players(mover).call(amount).map(newPs => this.copy(players = this.players.updated(mover, newPs)))
      }
    }
  }
}