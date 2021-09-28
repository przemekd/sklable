package shared

import shared.Board.Position
import shared.Board.Rows.Row
import shared.Sklable.Move
import shared.dto._

object GameProtocol {

  sealed trait GameMessage

  import upickle.default._

  object GameMessage {
    implicit val gameMessageRW: ReadWriter[GameMessage] = {
      implicit val tileRW: ReadWriter[Tile] = ReadWriter.merge(macroRW[Blank], macroRW[RegularTile])
      implicit val positionRW               = macroRW[Position]
      implicit val moveRW: ReadWriter[shared.Move] =
        ReadWriter.merge(
          macroRW[shared.Pass.type],
          macroRW[shared.InvalidMove.type],
          macroRW[shared.ExchangeTiles],
          macroRW[shared.PlayTiles],
          macroRW[shared.TimedOut.type]
        )
      implicit val playerDtoRW = macroRW[PlayerDTO]
      implicit val moveDtoRW   = macroRW[MoveDTO]
      implicit val rackRW      = macroRW[Rack]

      implicit val finishReasonRW: ReadWriter[shared.FinishReason] =
        ReadWriter.merge(
          macroRW[shared.ScorelessRounds.type],
          macroRW[shared.TimeOut.type],
          macroRW[shared.PlayingOut.type]
        )

      implicit val gameStateRW: ReadWriter[shared.GameState] =
        ReadWriter.merge(
          macroRW[shared.NotStarted.type],
          macroRW[shared.AboutToBeStarted],
          macroRW[shared.InProgress.type],
          macroRW[shared.Finished]
        )

      implicit val supportedLanguageRW: ReadWriter[shared.SupportedLanguage] = macroRW[shared.SupportedLanguage]
      implicit val gameConfigRW: ReadWriter[shared.GameConfig]               = macroRW[shared.GameConfig]

      implicit val gameRW = macroRW[GameDTO]

      implicit val joinedRoomRW       = macroRW[JoinedRoom]
      implicit val leftRoomRW         = macroRW[LeftRoom]
      implicit val joinGameRW         = macroRW[JoinGame.type]
      implicit val leaveGameRW        = macroRW[LeaveGame.type]
      implicit val joinedGameRW       = macroRW[JoinedGame]
      implicit val putTilesRW         = macroRW[PutTiles]
      implicit val updateConfigRW     = macroRW[UpdateConfig]
      implicit val exchangeTilesRW    = macroRW[ExchangeTiles]
      implicit val startGameRequestRW = macroRW[StartGameRequest.type]
      implicit val acceptStartGameRW  = macroRW[AcceptStartGameRequest.type]
      implicit val passRW             = macroRW[Pass.type]
      implicit val gameUpdateRW       = macroRW[GameUpdate]

      ReadWriter.merge(
        joinedRoomRW,
        leftRoomRW,
        joinGameRW,
        leaveGameRW,
        joinedGameRW,
        startGameRequestRW,
        acceptStartGameRW,
        putTilesRW,
        updateConfigRW,
        passRW,
        exchangeTilesRW,
        gameUpdateRW
      )
    }
  }

  case class JoinedRoom(id: String, name: String) extends GameMessage

  case class LeftRoom(id: String, name: String) extends GameMessage

  case class JoinedGame(sender: String) extends GameMessage

  case class GameUpdate(game: shared.dto.GameDTO, gameConfig: GameConfig, moderatorId: String) extends GameMessage

  case class GameState(inProgress: Boolean, players: List[String], previousMoves: List[Move]) extends GameMessage

  // Commands
  case object JoinGame                        extends GameMessage
  case object LeaveGame                       extends GameMessage
  case object StartGameRequest                extends GameMessage
  case object AcceptStartGameRequest          extends GameMessage
  case object Pass                            extends GameMessage
  case class ExchangeTiles(tiles: List[Tile]) extends GameMessage
  case class PutTiles(move: Move)             extends GameMessage
  case class UpdateConfig(config: GameConfig) extends GameMessage
  implicit val rowRW: ReadWriter[Row]           = macroRW[Row]
  implicit val positionRW: ReadWriter[Position] = macroRW[Position]
  implicit val tileRW: ReadWriter[Tile]         = ReadWriter.merge(macroRW[Blank], macroRW[RegularTile])

  //  implicit val moveRW: ReadWriter[Move] = macroRW[Move]
}
