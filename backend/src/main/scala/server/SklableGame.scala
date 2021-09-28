package server

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.{ ActorContext, Behaviors, TimerScheduler }
import shared.{
  Finished,
  Game,
  GameConfig,
  InProgress,
  NotStarted,
  PlayTiles,
  Player,
  PlayingOut,
  ScorelessRounds,
  SklableTimer,
  SupportedLanguage
}
import shared.Sklable.Move

import scala.concurrent.duration.FiniteDuration
import wvlet.log.LogSupport

import java.time.Instant
import java.time.Duration
import shared.AboutToBeStarted

object SklableGameCommands {
  sealed trait Command
  final case class JoinRoom(player: Player, actor: ActorRef[GameUpdate]) extends Command
  final case class LeaveRoom(player: Player)                             extends Command
// no active game
  final case object RoomInactive                                         extends Command
  final case class JoinGame(player: Player)                              extends Command
  final case class LeaveGame(player: Player)                             extends Command
  final case class ConfigureGame(player: Player, gameConfig: GameConfig) extends Command
  final case class StartGameRequest(player: Player)                      extends Command
  final case class AcceptStartGameRequest(player: Player)                extends Command
  final case class PlayersDidNotAccept(players: List[Player])            extends Command

// game in progress
  final case class TimeOut(player: Player)                                extends Command
  final case class PutTiles(player: Player, move: Move)                   extends Command
  final case class Pass(player: Player)                                   extends Command
  final case class ExchangeTiles(player: Player, move: List[shared.Tile]) extends Command
}

final case class GameUpdate(game: Game, gameConfig: GameConfig, moderatorId: String)

import SklableGameCommands._

object SklableGame extends LogSupport {

  def apply(roomId: String): Behavior[Command] = {
    debug(s"Creating room $roomId")
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        new SklableGame(roomId, context, timers).noActiveGame(Map.empty, Game())
      }
    }
  }
}

class SklableGame private (
    roomId: String,
    context: ActorContext[Command],
    akkaTimers: TimerScheduler[Command]
) extends LogSupport {

  private def noActiveGame(
      sessions: Map[Player, ActorRef[GameUpdate]],
      game: Game,
      gameConfig: GameConfig = GameConfig(5, SupportedLanguage.PL),
      moderatorId: Option[String] = None
  ): Behavior[Command] = {
    if (sessions.isEmpty)
      akkaTimers.startSingleTimer(
        "roomTimer",
        RoomInactive,
        FiniteDuration.apply(5, "minutes")
      )
    sessions.foreach { case (_, actor) => actor ! GameUpdate(game, gameConfig, moderatorId.getOrElse("")) }
    Behaviors.receiveMessage {
      case JoinRoom(player, actorRef) =>
        debug(s"$player joins in room[$roomId]")
        val modId = if (sessions.isEmpty) Some(player.id) else moderatorId
        akkaTimers.cancel("roomTimer")
        noActiveGame(sessions + (player -> actorRef), game, gameConfig, modId)
      case LeaveRoom(player) =>
        val modId = if (moderatorId.contains(player.id)) (sessions - player).headOption.map(_._1.id) else moderatorId
        if ((sessions - player).isEmpty)
          akkaTimers.startSingleTimer(
            "roomTimer",
            RoomInactive,
            FiniteDuration.apply(5, "minutes")
          )
        noActiveGame(sessions - player, game.removePlayers(List(player)), gameConfig, modId)
      case JoinGame(player) =>
        debug(s"$player join a game in room[$roomId]")
        val g = game.addPlayer(player)
        noActiveGame(sessions, g, gameConfig, moderatorId)
      case StartGameRequest(player) =>
        if (game.canBeStartedByPlayer(player)) {
          debug(s"$player starts a game in room[$roomId]")
          if (game.numberOfPlayers == 1)
            gameInProgress(
              sessions,
              game.start(gameConfig.minutes, gameConfig.language),
              gameConfig,
              moderatorId,
              Instant.now()
            )
          else
            startingGame(
              sessions,
              game.startRequested(player, 15),
              gameConfig,
              moderatorId,
              15
            )
        } else Behaviors.same
      case LeaveGame(player) =>
        noActiveGame(sessions, game.removePlayers(List(player)), gameConfig, moderatorId)
      case ConfigureGame(player, config) =>
        if (moderatorId.contains(player.id)) {
          debug(s"New game config [minutes: ${config.minutes}, lang: ${config.language.language}]")
          noActiveGame(sessions, game, config, moderatorId)
        } else Behaviors.same
      case RoomInactive => Behaviors.stopped
      case _            => Behaviors.same
    }
  }

  private def startingGame(
      sessions: Map[Player, ActorRef[GameUpdate]],
      game: Game,
      gameConfig: GameConfig,
      moderatorId: Option[String],
      waitFor: Int
  ): Behavior[Command] = {
    sessions.foreach { case (_, actor) => actor ! GameUpdate(game, gameConfig, moderatorId.getOrElse("")) }
        val (p, accept) = game.state match {
          case AboutToBeStarted(starter, needsToAccept, _) => (starter, needsToAccept)
          case _                                           => throw new IllegalStateException("Illegal game state")
        }
    akkaTimers.startSingleTimer(
      "sklableWaiting",
      PlayersDidNotAccept(game.players.filter(p => accept.contains(p.id)).toList),
      FiniteDuration.apply(waitFor, "seconds")
    )

    Behaviors.receiveMessage {
      case AcceptStartGameRequest(player) =>
        debug(s"$player accepts")
        val (p, accept) = game.state match {
          case AboutToBeStarted(starter, needsToAccept, _) => (starter, needsToAccept.filterNot(id => id == player.id))
          case _                                           => throw new IllegalStateException("Illegal game state")
        }
        if (accept.isEmpty)
          gameInProgress(
            sessions,
            game.start(gameConfig.minutes, gameConfig.language),
            gameConfig,
            moderatorId,
            Instant.now()
          )
        else
          startingGame(
            sessions,
            game.copy(state = AboutToBeStarted(p, accept, waitFor)),
            gameConfig,
            moderatorId,
            waitFor
          )
      case JoinRoom(player, actorRef) =>
        debug(s"$player joins in room[$roomId]")
        val modId = if (sessions.isEmpty) Some(player.id) else moderatorId
        startingGame(sessions + (player -> actorRef), game, gameConfig, modId, waitFor)
      case PlayersDidNotAccept(players) =>
        debug(s"players $players did not accept")
        noActiveGame(sessions, game.removePlayers(players).copy(state = NotStarted), gameConfig, moderatorId)
      case LeaveGame(player: Player) => // If players leave deactivate game
        noActiveGame(sessions, game.removePlayers(List(player)).copy(state = NotStarted), gameConfig, moderatorId)
      case LeaveRoom(player: Player) =>
        val modId = if (moderatorId.contains(player.id)) (sessions - player).headOption.map(_._1.id) else moderatorId
        if (sessions.contains(player))
          noActiveGame(sessions - player, game.removePlayers(List(player)).copy(state = NotStarted), gameConfig, modId)
        else {
          val modId = if (moderatorId.contains(player.id)) (sessions - player).headOption.map(_._1.id) else moderatorId
          startingGame(sessions - player, game, gameConfig, modId, waitFor)
        }
      case _ =>
        Behaviors.same
    }
  }

  private def gameInProgress(
      sessions: Map[Player, ActorRef[GameUpdate]],
      game: Game,
      gameConfig: GameConfig,
      moderatorId: Option[String],
      ts: Instant
  ): Behavior[Command] = {
    debug(s"Game in room[$roomId] started")
    sessions.foreach { case (_, actor) => actor ! GameUpdate(game, gameConfig, moderatorId.getOrElse("")) }

    def getUpdatedTimers(game: Game, ts: Instant) = {
      val newTs = Instant.now
      val diff  = Duration.between(ts, newTs).toMillis
      val id    = game.playerToMove.get
      game.timers.map(t => if (t._1 == id) (id, SklableTimer(t._2.millisecondsLeft - diff.toInt)) else t)
    }

    val playerToMove = game.playerToMove.get
    val timeLeft     = game.timers.find(_._1 == playerToMove).get._2.millisecondsLeft

    akkaTimers.startSingleTimer(
      "sklableTimer",
      TimeOut(game.players.find(_.id == playerToMove).get),
      FiniteDuration.apply(timeLeft, "millis")
    )

    Behaviors.receiveMessage {
      case StartGameRequest(_) => // Cannot start a game that is in progress
        Behaviors.same
      case AcceptStartGameRequest(_) => // Cannot start a game that is in progress
        Behaviors.same
      case PlayersDidNotAccept(_) => // whatever..
        Behaviors.same
      case JoinGame(_) => // Cannot join a game that is in progress
        Behaviors.same
      case LeaveGame(_) => // Cannot leave a game that is in progress
        Behaviors.same
      case ConfigureGame(_, _) => // Cannot change a game config when the game is in progress
        Behaviors.same
      case JoinRoom(player, actorRef) =>
        debug(s"$player joins in room[$roomId]")
        val modId  = if (sessions.isEmpty) Some(player.id) else moderatorId
        val timers = getUpdatedTimers(game, ts)
        gameInProgress(sessions + (player -> actorRef), game.copy(timers = timers), gameConfig, modId, Instant.now())
      case LeaveRoom(player) =>
        debug(s"$player leaves in room[$roomId]")
        val timers = getUpdatedTimers(game, ts)
        val modId  = if (moderatorId.contains(player.id)) (sessions - player).headOption.map(_._1.id) else moderatorId
        gameInProgress(sessions - player, game.copy(timers = timers), gameConfig, modId, Instant.now())
      case TimeOut(player) =>
        val maximumNoConsecutivePasses = game.players.length * 2
        val moves                      = game.moves :+ ((player.id, shared.TimedOut, 0))
        val lastMoves                  = moves.takeRight(maximumNoConsecutivePasses)
        val timers                     = game.timers.map(t => if (t._1 == player.id) (player.id, SklableTimer(0)) else t)
        if ((lastMoves.length == maximumNoConsecutivePasses) && lastMoves.forall(m => m._3 == 0)) {
          akkaTimers.cancelAll()
          noActiveGame(
            sessions,
            game.copy(state = Finished(ScorelessRounds), moves = moves),
            gameConfig,
            moderatorId = moderatorId
          )
        } else
          gameInProgress(sessions, game.copy(timers = timers, moves = moves), gameConfig, moderatorId, Instant.now)

      case PutTiles(player, move) =>
        val timers = getUpdatedTimers(game, ts)

        val previousMoves = game.moves
          .flatMap(m =>
            m._2 match {
              case PlayTiles(tiles) => Some(tiles)
              case _                => None
            }
          )
          .toList
        val wordsAndScores = shared.Sklable.getWordsAndScores(previousMoves, move)
        val allValid =
          wordsAndScores.map(_._1.map(_._1)).forall(word => WordChecker.wordExists(gameConfig.language, word))

        debug(s"In room $roomId played words: ${wordsAndScores.map(_._1.map(_._1.char).mkString)}.")

        if (allValid) {
          val points = shared.Sklable.calculatePoints(previousMoves, move)
          val moves  = game.moves :+ ((player.id, shared.PlayTiles(move), points))
          val rackTilesLeft = game.racks
            .find(rack => rack._1 == player.id)
            .get
            ._2
            .tiles
            .filterNot(tile => move.map(_._1.id).contains(tile.id))

          val rack = shared.Rack(game.tileBag.getTiles(7 - rackTilesLeft.length) :++ rackTilesLeft)

          val state = if (rack.tiles.isEmpty) Finished(PlayingOut) else InProgress

          val racks = game.racks.map(t => if (t._1 == player.id) (player.id, rack) else t)

          state match {
            case NotStarted => throw new IllegalStateException("Game cannot be in progress and have NotStarted state.")
            case AboutToBeStarted(_, _, _) =>
              throw new IllegalStateException("Game cannot be in progress and have AboutToBeStarted state.")
            case Finished(reason) =>
              akkaTimers.cancelAll()
              noActiveGame(
                sessions,
                game.copy(state = state, moves = moves, racks = racks, timers = timers),
                gameConfig,
                moderatorId
              )
            case InProgress =>
              gameInProgress(
                sessions,
                game.copy(state = state, moves = moves, racks = racks, timers = timers),
                gameConfig,
                moderatorId,
                Instant.now
              )
          }
        } else {
          val moves = game.moves :+ ((player.id, shared.InvalidMove, 0))
          gameInProgress(sessions, game.copy(moves = moves, timers = timers), gameConfig, moderatorId, Instant.now)
        }

      case Pass(player) =>
        val timers                     = getUpdatedTimers(game, ts)
        val moves                      = game.moves :+ ((player.id, shared.Pass, 0))
        val maximumNoConsecutivePasses = game.players.length * 2
        val lastMoves                  = moves.takeRight(maximumNoConsecutivePasses)
        if ((lastMoves.length == maximumNoConsecutivePasses) && lastMoves.forall(m => m._3 == 0)) {
          akkaTimers.cancelAll()
          noActiveGame(
            sessions,
            game.copy(state = Finished(ScorelessRounds), moves = moves, timers = timers),
            gameConfig,
            moderatorId
          )
        } else
          gameInProgress(sessions, game.copy(moves = moves, timers = timers), gameConfig, moderatorId, Instant.now)

      case ExchangeTiles(player, tiles) =>
        val timers = getUpdatedTimers(game, ts)
        debug(s"In room $roomId player wants to exchange tiles: ${tiles.map(_.char).mkString(", ")}")
        if (game.canExchangeTiles) {
          val moves = game.moves :+ ((player.id, shared.ExchangeTiles(tiles), 0))
          val rackTilesLeft = game.racks
            .find(rack => rack._1 == player.id)
            .get
            ._2
            .tiles
            .filterNot(tile => tiles.map(_.id).contains(tile.id))

          val newTiles = game.tileBag.exchangeTiles(tiles)
          val rack     = shared.Rack(newTiles :++ rackTilesLeft)
          val racks    = game.racks.map(t => if (t._1 == player.id) (player.id, rack) else t)

          gameInProgress(
            sessions,
            game.copy(moves = moves, racks = racks, timers = timers),
            gameConfig,
            moderatorId,
            Instant.now
          )
        } else {
          val moves = game.moves :+ ((player.id, shared.InvalidMove, 0))
          gameInProgress(sessions, game.copy(moves = moves, timers = timers), gameConfig, moderatorId, Instant.now)
        }

      case RoomInactive => Behaviors.same
    }

  }
}
