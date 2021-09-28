package game.ui.tabs

import wvlet.log.LogSupport
import com.raquo.laminar.api.L._
import locales.Languages
import game.ui.GameTab
import shared.Game

class PlayersTab(
    private val gameProgress: Signal[Game]
) extends LogSupport
    with GameTab {
  implicit val languages = Languages

  def render(): Div =
    div(
      cls := "players-tab",
      table(
        children <-- gameProgress.map { g =>
              g.players.map(p => p.name).sorted.map(div(_))
        }
      )
    )
}
