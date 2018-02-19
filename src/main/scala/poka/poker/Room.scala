package poka.poker

import poka.poker.Room.RoomError.{WrongMoveOrder, WrongTurnOrder}
import poka.poker.Room.RoomRound.PreFlop
import poka.poker.Room.{PlayerMove, PlayerState, RoomError, RoomRound}
import poka.poker.card.Card

object Room {
  sealed trait PlayerMove
  object PlayerMove {
    case object Fold extends PlayerMove
    case object Check extends PlayerMove
    case class Call(amount:Int) extends PlayerMove
  }

  def empty(deck:Deck) = Room(Map.empty, Set.empty, PreFlop, deck, Option.empty)

  case class PlayerState(putInAPot:Int, placeOnTable:Int, cards:Set[Card])

  sealed trait RoomRound
  object RoomRound {
    case object PreFlop extends RoomRound
    case class Flop(cards:(Card, Card, Card)) extends RoomRound
    case class Turn(card:Card) extends RoomRound
    case class River(card:Card) extends RoomRound
    case class Ended(result:Map[PlayerId, Int]) extends RoomRound
  }

  sealed trait RoomError
  object RoomError {
    case object WaitingForMorePlayers extends RoomError
    case object WrongTurnOrder extends RoomError
    case object WrongMoveOrder extends RoomError
    case object InternalGameLogicError extends RoomError
  }
}

case class PlayerId(id:String)

case class Room(players:Map[PlayerId, PlayerState], folded:Set[PlayerId], roomRound:RoomRound, deck:Deck, nextMover:Option[PlayerId]) {
  def joinPlayer(playerId:PlayerId):Room = {
    val newPlayer = (playerId -> PlayerState(0, this.players.size, Set.empty))
    if (roomRound == PreFlop) {
      this.copy(players = this.players + newPlayer)
    } else {
      this.copy(players=this.players + newPlayer, folded=this.folded+playerId)
    }
  }

  def playerMove(playerId:PlayerId, move:PlayerMove):Either[RoomError, Room] = {
    require(players.size >= 2) // TODO: replace with guard
    require((nextMover.isDefined && players.contains(nextMover.get) && folded.contains(nextMover.get)) || roomRound == PreFlop) // TODO: replace with guard
    require(!folded.contains(playerId)) // TODO: to guard

    val next = nextMover.getOrElse(players.minBy(_._2.placeOnTable)._1)
    if (playerId != next) Left(WrongTurnOrder)
    else {
      move match {
        // one can always fold
        case PlayerMove.Fold =>
          Right(this.copy(folded=this.folded + playerId)) // TODO :implement me
      }
      // previous players bet should be called or you fold
      // in case if no bets have been placed one can check

      //val previousPlayers
      ???
    }
  }
}


