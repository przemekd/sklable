package server

import akka.actor.{ Actor, ActorRef }
import akka.http.scaladsl.model.ws.TextMessage
import upickle.default.write
import shared.GameProtocol
import wvlet.log.LogSupport
import akka.actor.typed.scaladsl.adapter._

class PlayerActor(player: shared.Player, webSocket: ActorRef) extends Actor with LogSupport {

  import AckingReceiver._
  import server.SklableGameCommands._

  var maybeRoom: Option[akka.actor.typed.ActorRef[server.SklableGameCommands.Command]] = None

  // TODO: Handle WS Messages represented as stream:
  // https://doc.akka.io/docs/akka-http/current/server-side/websocket-support.html#model

  //This will schedule to send the Tick-message
  //to the tickActor after 0ms repeating every 50ms
  override def receive: Receive = {
    case Ack => println("Ack...")

    case StreamInitialized =>
      println("Stream initialized")
      sender() ! Ack
    case StreamCompleted =>
      println("Stream completed")
      this.maybeRoom.map(_ ! LeaveRoom(player))
      sender() ! Ack

    case SklableManagerMessage.CreatedRoom(room) =>
      this.maybeRoom = Some(room)
      this.maybeRoom.map(_ ! JoinRoom(player, self.toTyped))
      debug("Created room and can join")
    case TextMessage.Strict(value) =>
      println(value)
      import upickle.default._

      val msg = read[GameProtocol.GameMessage](value)

      msg match {
        case c: GameProtocol.JoinGame.type =>
          debug(s"Player ${player.id} wants to join a game.")
          maybeRoom.map(_ ! JoinGame(player))
        case c: GameProtocol.LeaveGame.type =>
          debug(s"Player ${player.id} wants to leave a game.")
          maybeRoom.map(_ ! LeaveGame(player))
        case c: GameProtocol.StartGameRequest.type =>
          debug(s"Player $player wants to start a game.")
          maybeRoom.map(_ ! StartGameRequest(player))
        case c: GameProtocol.AcceptStartGameRequest.type =>
          debug(s"Player $player accepts game start.")
          maybeRoom.map(_ ! AcceptStartGameRequest(player))
        case c: GameProtocol.Pass.type =>
          debug(s"Player $player wants to pass.")
          maybeRoom.map(_ ! Pass(player))
        case GameProtocol.PutTiles(move) =>
          debug(s"Player $player wants to make a move.")
          maybeRoom.map(_ ! PutTiles(player, move))
        case GameProtocol.ExchangeTiles(tiles) =>
          debug(s"Player $player wants to exchange tiles.")
          maybeRoom.map(_ ! ExchangeTiles(player, tiles))
        case GameProtocol.UpdateConfig(config) =>
          debug(s"Player $player wants to update config.")
          maybeRoom.map(_ ! ConfigureGame(player, config))
        case _ => debug(s"Woot?")
      }
      sender() ! Ack
    case StreamFailure => println("Stream failure")

    case GameUpdate(game, gameConfig, moderatorId) =>
      debug("Sending Game Update")
      webSocket ! TextMessage(write[GameProtocol.GameMessage](GameProtocol.GameUpdate(game, gameConfig, moderatorId)))
    case msg =>
      println(msg)
      sender() ! Ack
    // room ! msg
  }
}
