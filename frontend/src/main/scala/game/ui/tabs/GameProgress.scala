package game.ui.tabs

import wvlet.log.LogSupport
import com.raquo.laminar.api.L._
import locales.Languages
import game.ui.GameTab
import shared.Game
import shared.Pass
import shared.InvalidMove
import shared.TimedOut
import shared.ExchangeTiles
import shared.PlayTiles
import shared.ProposedPlay

class GameProgress(
    private val gameProgress: Signal[Game]
) extends LogSupport
    with GameTab {
  implicit val languages = Languages

  def render(): Div =
    div(
      cls := "game-progress",
      table(
        children <-- gameProgress.map { g =>
              val noPlayers = g.playOrder.length
              if (noPlayers == 0)
                List(tr())
              else {
                val moves = g.moves.sliding(noPlayers, noPlayers).toList
                moves.map(r =>
                  tr(
                    r.map(m =>
                      m._2 match {
                        case Pass                 => td("(P)")
                        case InvalidMove          => td("(I)")
                        case TimedOut             => td("(TO)")
                        case ExchangeTiles(tiles) => td("(W)")
                        case PlayTiles(tiles)     => td(s"+${m._3}")
                        case ProposedPlay(_)      => td()
                      }
                    )
                  )
                )
              }
            }
      )
    )
}
