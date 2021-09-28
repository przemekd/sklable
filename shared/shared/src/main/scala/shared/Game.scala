package shared

import scala.util.Random

final case class Player(id: String, name: String)

final case class Game(
    state: GameState,
    players: Seq[Player],
    playOrder: Seq[Player],
    racks: Seq[(String, Rack)],
    timers: Seq[(String, SklableTimer)],
    moves: Seq[(String, Move, Int)],
    tileBag: TileBag
) {
  def playerToMove: Option[String] =
    state match {
      case InProgress =>
        moves.lastOption.map(move => move._1) match {
          case Some(id) =>
            val idx = playOrder.indexWhere(p => p.id == id)
            if (idx == playOrder.length - 1) playOrder.headOption.map(_.id)
            else Some(playOrder(idx + 1)).map(_.id)
          case None => playOrder.headOption.map(_.id)
        }
      case _ => None
    }

  def addPlayer(player: Player) =
    if (this.canJoin(player.id))
      this.copy(players = this.players :+ player)
    else
      this

  def removePlayers(players: List[Player]) =
    this.copy(players = this.players.filterNot(p => players.map(_.id).contains(p.id)))

  def startRequested(player: Player, waitFor: Int) =
    this.copy(state = AboutToBeStarted(player.id, this.players.map(_.id).filterNot(id => id == player.id), waitFor))

  def start(minutes: Int, language: SupportedLanguage) = {
    val newTileBag = TileBag.getTileBagByLang(language)
    val racks      = players.map(player => (player.id, Rack(newTileBag.getTiles(7))))
    val timers     = players.map(player => (player.id, SklableTimer(60 * 1000 * minutes)))

    Game(
      InProgress,
      this.players,
      Random.shuffle(this.players),
      racks,
      timers,
      Seq.empty[(String, Move, Int)],
      newTileBag
    )
  }

  def numberOfPlayers: Int = players.length

  def isInProgress: Boolean =
    state match {
      case NotStarted                => false
      case AboutToBeStarted(_, _, _) => true
      case InProgress                => true
      case Finished(reason)          => false
    }

  def canJoin(id: String): Boolean =
    !isInProgress && numberOfPlayers < Game.maxPlayers && !players.map(_.id).contains(id)

  def canBeStarted: Boolean = !isInProgress && numberOfPlayers >= Game.minPlayers

  def canBeStartedByPlayer(player: Player): Boolean = canBeStarted && this.players.contains(player)

  def canExchangeTiles: Boolean = tileBag.numberOfTilesLeft >= 7
}

object Game {
  val minPlayers = 1
  val maxPlayers = 4

  def apply(language: SupportedLanguage = SupportedLanguage.EN): Game =
    Game(
      NotStarted,
      Seq.empty[Player],
      Seq.empty[Player],
      Seq.empty[(String, Rack)],
      Seq.empty[(String, SklableTimer)],
      Seq.empty[(String, Move, Int)],
      TileBag.getTileBagByLang(language)
    )
}

sealed trait FinishReason
case object ScorelessRounds extends FinishReason
case object TimeOut         extends FinishReason
case object PlayingOut      extends FinishReason

sealed trait GameState
case object NotStarted                                                                                 extends GameState
case class AboutToBeStarted(requestedBy: String, needsToAccept: Seq[String], secondsForResponses: Int) extends GameState
case object InProgress                                                                                 extends GameState
case class Finished(reason: FinishReason)                                                              extends GameState
