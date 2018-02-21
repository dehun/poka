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
    case class Flop(cards:(Card, Card, Card)) extends RoomRound {
      lazy val allCards:Set[Card] = Set(cards._1, cards._2, cards._3)
    }
    case class Turn(card:Card, flop: Flop) extends RoomRound {
      lazy val allCards:Set[Card] = flop.allCards + card
    }
    case class River(card:Card, turn: Turn) extends RoomRound {
      lazy val allCards:Set[Card] = turn.allCards + card
    }
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

    def takePot(maxShare:Int):(PlayerState, Int) = {
      require(maxShare >= 0)

      (this.copy(putInAPot=math.max(this.putInAPot - maxShare, 0)),
        math.min(maxShare, putInAPot))
    }

    def winPot(win:Int):PlayerState = ???
  }
}

case class PlayerId(id:String)

case class Room(players:Map[PlayerId, PlayerState], folded:Set[PlayerId], roomRound:RoomRound, deck:Deck, nextMover:Option[PlayerId]) {
  def joinPlayer(playerId:PlayerId, stack:Stack):Room = {
    if (roomRound == PreFlop) {
      val (newDeck, cards) = deck.pollN(2).get // TODO either
      val newPlayer = playerId -> PlayerState(0, stack, this.players.size, cards.toSet)
      this.copy(players = this.players + newPlayer, deck=newDeck)
    } else {
      val newPlayer = playerId -> PlayerState(0, stack, this.players.size, Set.empty)
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
      } yield this.copy(roomRound = RoomRound.Flop((f, s, t)), deck = newDeck)
    }.get // TODO: either
    case flop:RoomRound.Flop =>
      val (newDeck, card) = deck.poll.get
      this.copy(roomRound=RoomRound.Turn(card, flop), deck=newDeck)
    case turn:RoomRound.Turn =>
      val (newDeck, card) = deck.poll.get
      this.copy(roomRound=RoomRound.River(card, turn), deck=newDeck)
    case river:RoomRound.River =>
      val handsInOrder = (inGamePlayers ++ allInPlayers)
        .map{pid => (pid, Hand(players(pid).cards, river.allCards))}.toVector
        .sortBy {case (pid, hand) => hand}
        .map {case (pid, hand) => pid}
        .zipWithIndex.toMap
      val newPlayerStates = handsInOrder.keySet.foldLeft(players) { (initialNewPlayers, pid) =>
        val winnerShare = initialNewPlayers(pid).putInAPot
        initialNewPlayers.foldLeft(initialNewPlayers) {case (newPlayers, (lpid, lps)) =>
          if (folded.contains(lpid) || (handsInOrder(lpid) < handsInOrder(pid))) {
            val (newLps, potTakken) = lps.takePot(winnerShare)
            newPlayers
              .updated(lpid, newLps)
              .updated(pid, newPlayers(pid).winPot(potTakken))
          } else newPlayers
        }
      }

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