package shared.dto

import shared.GameState
import shared.Game
import shared.Rack
import shared.TileBag
import shared.SklableTimer

case class GameDTO(state: GameState, players: Seq[PlayerDTO], playOrder: Seq[PlayerDTO], moves: Seq[MoveDTO], timers: Seq[(String, Int)], racks: Seq[(String, Rack)], tilesLeft: Int)

object GameDTO {
  import scala.language.implicitConversions

  implicit def gameDtoToGame(gameDto: GameDTO): Game =
    Game(
      gameDto.state,
      gameDto.players.map(p => shared.Player(p.id, p.name)),
      gameDto.playOrder.map(p => shared.Player(p.id, p.name)),
      gameDto.racks,
      gameDto.timers.map(t => (t._1, SklableTimer(t._2))),
      gameDto.moves.map(move => (move.playerId, move.move, move.points)),
      TileBag.builder().addTiles("", -1, gameDto.tilesLeft).build
    )

  implicit def gameToGameDto(game: Game): GameDTO =
    GameDTO(
      game.state,
      game.players.map(p => PlayerDTO(p.id, p.name)),
      game.playOrder.map(p => PlayerDTO(p.id, p.name)),
      game.moves.map(move => MoveDTO(move._1, move._2, move._3)),
      game.timers.map(t => (t._1, t._2.millisecondsLeft)),
      game.racks,
      game.tileBag.numberOfTilesLeft
    )
}
