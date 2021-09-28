import game._
import org.scalajs.dom
import org.scalajs.dom.raw._
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import services.{ I18nService, PlayerService }
import shared.{ Blank, Game, GameProtocol, Sklable }
import util._
import upickle.default._
import wvlet.log.LogSupport
import shared.PlayTiles
import shared.InvalidMove

import scala.concurrent.Future
import shared.SklableTimer

import scala.scalajs.js.timers._
import shared.TimedOut
import shared.GameConfig

import scala.util.{ Failure, Success }
import wvlet.log.Logger
import wvlet.log.LogLevel
import shared.NotStarted
import shared.InProgress
import shared.Finished
import game.ui.{ GameInfoTab, GameSettings, GameTabs}
import ru.makkarpov.scalingua.I18n.{ stringContext2Interpolator, t, p => pl }
import game.ui.tabs.GameProgress
import shared.ProposedPlay
import game.ui.SolidMove
import game.ui.BlinkingMove
import shared.AboutToBeStarted
import game.ui.tabs.PlayersTab
import game.ui.AcceptGameModal

class SklableGame(roomId: String) extends LogSupport {
  // BUG: On reconnecting to the room there is no Game Update
  // TODO: Implement websockets client https://github.com/jrudolph/akka-http-scala-js-websocket-chat
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  implicit val m          = locales.Languages
  implicit val languageId = I18nService.getLanguage()

  Logger.setDefaultLogLevel(LogLevel.DEBUG)
  debug(s"Joining Sklable room: $roomId")

  val dragEventBus     = new EventBus[DraggableEvent]
  val gameInteractions = new EventBus[GameEvent]

  var gameState: Var[GameState] = Var[GameState](PlayerMove)
  // TODO: Introduce GameState with States, players etc
  var game: Var[Game] = Var[Game](shared.Game())

  var gameConfig: Var[GameConfig] = Var[GameConfig](shared.GameConfig())

  val maybeDraggedTile: Var[Option[DraggedTile]] = Var[Option[DraggedTile]](None)

  val playersInGame = game.signal.map(s => s.players)

  val playersPoints = game.signal.map(g => g.moves.map(m => (m._1, m._3)).groupBy(_._1).view.mapValues(_.map(_._2).sum))

  val canPlayerJoin = game.signal.map(game => game.canJoin(PlayerService.getLocalPlayer().now().id.toString))

  val canInteract = game.signal.map(game => !game.isInProgress)

  val gameModerator = Var[String]("")

  var canConfig = gameModerator.signal.combineWith(game.signal).map {
    case (id, game) => PlayerService.getLocalPlayer().now().id.toString == id && !game.isInProgress
  }

  val canPlayerStartGame = game.signal.map { game =>
    val player = PlayerService.getLocalPlayer().now()
    game.canBeStartedByPlayer(shared.Player(player.id.toString, player.name))
  }

  val rack          = new Rack(maybeDraggedTile.writer, dragEventBus, gameInteractions)
  val board         = new Board(maybeDraggedTile.writer, dragEventBus, gameInteractions, gameConfig.signal.map(_.language))
  val gameUpdates   = new GameInfoTab(200)
  val gameProgress  = new GameProgress(game.signal)
  val playersInRoom = new PlayersTab(game.signal)
  val acceptGameModal = new AcceptGameModal(game.signal.map(_.state))

  val draggedTile = Var[Option[ReactiveHtmlElement[dom.html.Div]]](None)

  def joinGame() = {
    val localPlayer = PlayerService.getLocalPlayer()
    game.update(g => g.addPlayer(shared.Player(localPlayer.now().id.toString, localPlayer.now().name)))

    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.JoinGame)
    gameWs.foreach(_.send(msg))
  }

  def leaveGame() = {
    val localPlayer = PlayerService.getLocalPlayer()
    game.update(g => g.removePlayers(List(shared.Player(localPlayer.now().id.toString, localPlayer.now().name))))

    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.LeaveGame)
    gameWs.foreach(_.send(msg))
  }

  def requestGameStart() = {
    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.StartGameRequest)
    gameWs.foreach(_.send(msg))
  }

  def acceptGameStart() = {
    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.AcceptStartGameRequest)
    gameWs.foreach(_.send(msg))
  }

  def sendMove(move: Sklable.Move) = {
    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.PutTiles(move))
    gameWs.foreach(_.send(msg))
  }

  def pass() = {
    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.Pass)
    gameWs.foreach(_.send(msg))
  }

  def exchangeTiles(oldTiles: List[shared.Tile]) = {
    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.ExchangeTiles(oldTiles))
    debug(s"Exchanging tiles ${oldTiles.map(_.char).mkString(", ")}")
    gameWs.foreach(_.send(msg))
  }

  def updateConfig(config: GameConfig) = {
    import shared.GameProtocol.GameMessage._
    val msg = write[GameProtocol.GameMessage](GameProtocol.UpdateConfig(config))
    debug(s"Updating config $config")
    gameWs.foreach(_.send(msg))
  }

  val gameSettings = new GameSettings(gameConfig, canConfig, updateConfig, languageId)
  val gameTabs = new GameTabs(
    List(
      (languageId.map { lang => implicit val id = lang; t("CHAT") }, gameUpdates),
      (languageId.map { lang => implicit val id = lang; t("GAME COURSE") }, gameProgress),
      (languageId.map { lang => implicit val id = lang; t("PLAYERS") }, playersInRoom),
      (languageId.map { lang => implicit val id = lang; t("SETTINGS") }, gameSettings)
    )
  )

  val maybeDraggedHTMLElement = maybeDraggedTile.signal.map {
    case Some(el) =>
      val element = el.render()
      rack.removeTile(el.tile.id)
      board.removeTile(el.tile.id)
      draggedTile.update(_ => Some(element))
      element
    case None =>
      draggedTile.update(_ => None)
      div()
  }

  def getWebsocketsUri: Future[String] = {
    val localPlayer = PlayerService.getLocalPlayer()
    val maybeToken  = PlayerService.getToken(localPlayer.now().id.toString, localPlayer.now().secret)
    val wsProtocol  = if (dom.document.location.protocol == "https:") "wss" else "ws"

    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    maybeToken.map(token => s"$wsProtocol://${dom.document.location.host}/ws/room/$roomId?token=$token")
  }

  def joinRoom() = {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    import shared.GameProtocol.GameMessage._

    def writeToArea(text: String): Unit =
      gameUpdates.addMessage(text)

    val roomConnection = for {
      uri <- getWebsocketsUri
    } yield new WebSocket(uri)

    roomConnection.onComplete {
      case Failure(exception) =>
        gameUpdates.addMessage(s"Room connection was unsuccessful(${exception.getLocalizedMessage})")
      case Success(r) =>
        r.onopen = { (event: Event) =>
          implicit val id = languageId.now()
          gameUpdates.addMessage(t"Room connection was successful")

          event
        }
        r.onerror = { (event: Event) =>
          gameUpdates.addMessage(s"Failed: code: ${event.asInstanceOf[ErrorEvent].colno}")
        }
        r.onmessage = { (event: MessageEvent) =>
          val wsMsg       = read[shared.GameProtocol.GameMessage](event.data.toString)
          implicit val id = languageId.now()

          wsMsg match {
            case GameProtocol.JoinedRoom(sender, message) => writeToArea(t"$sender joined!")
            case GameProtocol.LeftRoom(member, _)         => writeToArea(t"$member left!")
            case GameProtocol.GameUpdate(gameUpdate, config, moderator) =>
              if (moderator != gameModerator.now()) {
                gameModerator.set(moderator)
                val localPlayer = PlayerService.getLocalPlayer().now()
                if (localPlayer.id.toString == moderator)
                  writeToArea(t"You became the game moderator.")
              }

              if (gameConfig.now().minutes != config.minutes)
                writeToArea(
                  pl(
                    "Game time set to %(n) minute.",
                    "Game time set to %(n) minutes.",
                    config.minutes
                  )
                )

              gameConfig.set(config)

              if (gameUpdate.moves.length != game.now().moves.length)
                // TODO: Check if it works if one changes his name
                gameUpdate.moves.lastOption match {
                  case Some(move) =>
                    gameUpdate.players.find(_.id == move.playerId).map(_.name) match {
                      case Some(name) =>
                        move.move match {
                          case shared.Pass => writeToArea(t"Player $name passed.")
                          case InvalidMove => writeToArea(t"Player $name made an invalid move.")
                          case shared.ExchangeTiles(tiles) =>
                            writeToArea(
                              pl(
                                "Player %(name) exchanged %(n) tile.",
                                "Player %(name) exchanged %(n) tiles.",
                                tiles.length,
                                "name" -> name
                              )
                            )
                          case ProposedPlay(_) =>
                          case PlayTiles(_)    =>
                          case TimedOut =>
                            val lastPlayerMoves =
                              gameUpdate.moves.filter(m => m.playerId == move.playerId).takeRight(2)
                            if (
                              lastPlayerMoves.length == 1 || !lastPlayerMoves.head.move
                                .isInstanceOf[shared.TimedOut.type]
                            )
                              writeToArea(t"Player $name timed out.")
                        }
                      case None =>
                    }
                  case None =>
                }

              if (gameUpdate.state != game.now().state)
                gameUpdate.state match {
                  case NotStarted =>
                  case InProgress => writeToArea(t"Game started.")
                  case AboutToBeStarted(_, needsToAccept, _) =>
                    writeToArea(t"Waiting for others to accept...")
                    writeToArea(s"${needsToAccept}")
                    println(needsToAccept)
                  case Finished(_) =>
                    val gameScores =
                      gameUpdate.moves.groupBy(m => m.playerId).map(m => (m._1, m._2.map(_.points).sum))
                    val maximumScore = gameScores.values.max
                    val winners      = gameScores.filter(_._2 == maximumScore).keys.toList
                    if (winners.length == 1)
                      gameUpdate.players.find(_.id == winners.head).map(_.name) match {
                        case Some(name) => writeToArea(t"Game ended. Player $name won!")
                        case None       =>
                      }
                    else
                      writeToArea(t"Game ended. Draw.")

                }

              game.set(gameUpdate)

              game.now().playerToMove match {
                case Some(value) if value == PlayerService.getLocalPlayer().now().id.toString =>
                  SoundPlayer.playNotificationSound()
                  gameInteractions.writer.onNext(PlayerTurn)
                case _ =>
                  gameInteractions.writer.onNext(Observe)
              }
              board.updateMoves(
                gameUpdate.moves.flatMap { m =>
                  val lastMove = gameUpdate.moves.lastOption
                  m.move match {
                    case shared.PlayTiles(tiles) =>
                      if (m == lastMove.get)
                        Some(BlinkingMove(tiles))
                      else
                        Some(SolidMove(tiles))

                    case shared.ProposedPlay(tiles) => Some(BlinkingMove(tiles))
                    case _                          => None
                  }
                }.toList
              )

              gameUpdate.state match {
                case NotStarted                      =>
                case InProgress                      =>
                case AboutToBeStarted(_, needsToAccept, _) =>
                case Finished(reason) =>
                  board.updateMove(List.empty)
              }
              // FIXME: New game
              val maybeRack =
                gameUpdate.racks.find(p => p._1 == PlayerService.getLocalPlayer().now().id.toString).map(_._2)
              maybeRack match {
                case Some(value) =>
                  maybeDraggedTile.now() match {
                    case None => rack.putTiles(value.tiles.toList)
                    case Some(tile) =>
                      val tiles = value.tiles.filterNot(t => t.id == tile.tile.id).toList
                      rack.putTiles(tiles)
                  }
                case None =>
              }
            case msg => warn(s"Unknown message $msg")
          }
        }
        r.onclose = { (event: Event) =>
          // TODO: Reconnect automatically
          gameUpdates.addMessage("Connection to room lost. You can try to rejoin manually.")
        }
    }

    roomConnection
  }

  val gameWs: Future[WebSocket] = joinRoom()

  def Game(): Div = {
    val gameDiv = div(
      height := "100%",
      child <-- maybeDraggedHTMLElement,
      // Sometimes Chrome doesn't capture pointer
      inContext { thisNode =>
        onPointerUp --> (ev => {
          maybeDraggedTile.now() match {
            case Some(value) =>
              ev.stopPropagation()
              // debug(s"Drag end, pointerId[${ev.pointerId}]")

              val elementsBelow = DOMUtils.elementsFromPoint(ev.clientX, ev.clientY)
              val elemBelow     = elementsBelow.find(el => el != null && el.classList.contains("droppable"))

              elemBelow match {
                case Some(el) =>
                  val dropZonePosition = el.getAttribute("data-dropzone-position")
                  val dropZone         = el.getAttribute("data-dropzone-class")
                  dragEventBus.writer.onNext(DropInto(dropZone, dropZonePosition, value.draggableTile))
                case None => dragEventBus.writer.onNext(OutOfDropzone)
              }

            case None =>

          }
        })
      },
      div(
        cls := "game",
        div(
          cls := "boardAndRack",
          board.render(),
          rack.render(),
          acceptGameModal.render(() => acceptGameStart())
        ),
        div(
          cls := "controls",
          div(
            cls := "players-window",
            div(
              cls := "players-window-header",
              "[P: ",
              child <-- playersInGame.signal.map(players => span(players.length.toString)),
              ", ",
              "T: ",
              child <-- game.signal.map(game => span(game.tileBag.numberOfTilesLeft.toString)),
              "]",
              "       "
            ),
            div(
              cls := "players-window-list",
              children <-- playersInGame
                    .combineWith(playersPoints)
                    .combineWith(game.signal)
                    .signal
                    .map {
                      case (players, points, game) =>
                        players
                          .map(p =>
                            renderPlayer(
                              game.playerToMove.contains(p.id),
                              p.name,
                              points.getOrElse(p.id, 0),
                              game.timers.find(_._1 == p.id).map(_._2).getOrElse(SklableTimer(0))
                            )
                          )
                    }
            )
          ),
          div(
            // TODO: Controls should work for all states
            cls := "game-controls-window",
            child <-- canInteract.map(b =>
                  if (b)
                    div(
                      cls := "gameControls",
                      child <-- canPlayerJoin.map { canJoin =>
                            if (canJoin)
                              button(
                                child <-- languageId.map(implicit id => t("Join Game")),
                                onClick --> (_ => joinGame())
                              )
                            else
                              button(
                                child <-- languageId.map(implicit id => t("Leave Game")),
                                onClick --> (_ => leaveGame())
                              )
                          },
                      button(
                        child <-- languageId.map(implicit id => t("New Game")),
                        disabled <-- canPlayerStartGame.map(!_),
                        onClick --> (_ => requestGameStart())
                      )
                    )
                  else div()
                ),
            child <-- gameInteractions.events.toWeakSignal.map {
                  case Some(ev) =>
                    ev match {
                      case PlayerTurn =>
                        div(
                          cls := "gameControls",
                          button(
                            child <-- board.getMove().signal.combineWith(board.getPreviousMoves().signal).map {
                                  case (move, previousMoves) =>
                                    val points = Sklable.calculatePoints(previousMoves.map(_.move), move)
                                    if (points > 0) s"(+ $points) ok" else "ok"
                                },
                            disabled <-- board.isValidMoveAdded().map(!_),
                            onClick --> (_ => gameInteractions.writer.onNext(AcceptMove))
                          ),
                          button(
                            child <-- languageId.map(implicit id => t("exchange")),
                            disabled <-- game.signal.map(game => !game.canExchangeTiles),
                            onClick --> (_ => gameInteractions.writer.onNext(ProposeExchangeTiles(List.empty)))
                          ),
                          button(
                            child <-- languageId.map(implicit id => t("pass")),
                            onClick --> (_ => gameInteractions.writer.onNext(Pass))
                          )
                        )
                      case AcceptMove           => div()
                      case Pass                 => div()
                      case WaitingForChallenge  => div()
                      case GetBlankChar(tileId) =>
                        // TODO: Refactor and join with ProposeBlankChar below
                        div(
                          cls := "gameControls",
                          select(
                            option(value := " ", " ", unselectable := true),
                            // FIXME: It should work for all languages
                            shared.TileBag
                              .getTileBagByLang(gameConfig.signal.now().language)
                              .getChars
                              .map(c => option(value := c, c)),
                            onChange --> (ev => {
                                  val target = ev.target.asInstanceOf[HTMLSelectElement]
                                  val picked = target.options.map(_.value)(target.selectedIndex)
                                  if (picked.trim.nonEmpty)
                                    gameInteractions.writer.onNext(ProposeBlankChar(tileId, picked))
                                })
                          ),
                          button("ok", disabled := true)
                        )
                      case ProposeBlankChar(tileId, string) =>
                        div(
                          cls := "gameControls",
                          select(
                            shared.TileBag
                              .getTileBagByLang(gameConfig.signal.now().language)
                              .getChars
                              .map(c => option(value := c, c, selected := c == string)),
                            onChange --> (ev => {
                                  val target = ev.target.asInstanceOf[HTMLSelectElement]
                                  val picked = target.options.map(_.value)(target.selectedIndex)
                                  if (picked.trim.nonEmpty)
                                    gameInteractions.writer.onNext(ProposeBlankChar(tileId, picked))
                                })
                          ),
                          button("ok", onClick --> (_ => nextRound()))
                        )
                      case ProposeExchangeTiles(tiles) =>
                        div(
                          cls := "gameControls",
                          button(
                            "ok",
                            disabled := tiles.isEmpty,
                            onClick --> (_ => gameInteractions.writer.onNext(ExchangeTiles(tiles)))
                          ),
                          button(
                            child <-- languageId.map(implicit id => t("cancel")),
                            onClick --> (_ => gameInteractions.writer.onNext(PlayerTurn))
                          )
                        )
                      case state =>
                        warn(s"Unknown state $state")
                        div()
                    }
                  case None => div()
                }
          ),
          div(
            cls := "stage-observer",
            dataAttr("game-state") <-- gameInteractions.events.map {
                  case WaitingForChallenge  => "challange"
                  case GetBlankChar(tileId) => "getting blank"
                  case ProposeBlankChar(tileId, string) =>
                    gameState.set(BlankProposed(tileId, string))
                    "blank proposed"
                  case AcceptMove =>
                    gameInteractions.writer.onNext(PlayerTurn)
                    nextRound()
                    "accept"
                  case PlayerTurn => "player-turn"
                  case Pass =>
                    val m = board.getMove().now()
                    board.getMove().set(List.empty)
                    rack.addTiles(m.map(t => t._1))
                    pass()
                    "pass"
                  case ProposeExchangeTiles(tiles) =>
                    if (tiles.isEmpty && board.getMove().now().nonEmpty) {
                      val m = board.getMove().now()
                      board.getMove().set(List.empty)
                      rack.addTiles(m.map(t => t._1))
                    }
                    "exchange"
                  case ExchangeTiles(oldTiles) =>
                    // TODO: Exchange only if possible
                    rack.removeTiles(oldTiles)
                    exchangeTiles(oldTiles)
                    "exchanged"
                  case state =>
                    warn(s"Unknown state $state")
                    "unknown"
                }
          ),
          gameTabs.render()
        )
      )
    )

    gameDiv
  }

  def renderPlayer(isPlayerRound: Boolean, playerName: String, points: Int, timer: SklableTimer) = {
    def renderMarker =
      div(
        cls := "player-marker",
        "•"
      )

    div(
      cls := "player-window-player",
      if (isPlayerRound) renderMarker else div(cls := "player-marker"),
      div(
        cls := "player-window-name",
        span(playerName)
      ),
      div(
        cls := "player-window-score",
        span(points.toString)
      ),
      renderSklableTimer(timer, isPlayerRound)
    )
  }

  def renderSklableTimer(timer: SklableTimer, isActive: Boolean = false) = {
    val t = Var[SklableTimer](timer)
    val maybeInterval =
      if (isActive)
        Some(setInterval(1000.0)(t.update(s => SklableTimer(s.millisecondsLeft - 1000))))
      else
        None

    div(
      cls := "player-window-time",
      child <-- t.signal.map(timer => span(timer.toString)),
      onUnmountCallback(thisNode =>
        maybeInterval match {
          case None         =>
          case Some(handle) => clearInterval(handle)
        }
      )
    )
  }

  def nextRound(): Unit = {
    debug("Space pressed")

    if (
      maybeDraggedTile
        .now()
        .isEmpty && shared.Sklable.isValidMove(board.getPreviousMoves().now().map(_.move), board.getMove().now())
    ) {

      val move = board.getMove().now()

      gameState.now() match {
        case PlayerMove =>
          debug("Player Turn")
          playerEvaluation()
        case WaitingForBlank(tileId) => debug(s"Waiting for Blank $tileId")
        case BlankProposed(tileId, c) =>
          board.getMove().update(m => m.map(t => if (t._1.id == tileId) (Blank(tileId, Some(c)), t._2) else t))
          gameState.set(PlayerMove)
          nextRound()
      }

      def playerEvaluation() =
        move.find { case (tile, _) => debug(s"${tile.char}"); tile.char.trim.isEmpty } match {
          case Some(value) =>
            gameInteractions.writer.onNext(GetBlankChar(value._1.id))
            gameState.set(WaitingForBlank(value._1.id))
          case None =>
            gameInteractions.writer.onNext(Observe)
            sendMove(move)
            board.freeze()
        }

    }
  }

}
