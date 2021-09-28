package game.ui

import com.raquo.laminar.api.L._
import wvlet.log.LogSupport

trait GameTab {
  def render(): Div
}

class GameTabs(tabs: List[(Signal[String], GameTab)]) extends LogSupport {
  private val tabActive = Var(0)

  def render(): Div =
    div(
      cls := "game-tabs",
      div(
        cls := "tab-bar",
        tabs.zipWithIndex.map(t =>
          div(
            cls := "tab-link",
            cls <-- tabActive.signal.map(a => if (a == t._2) "tab-link-active" else ""),
            child <-- t._1._1.map(span(_)),
            onClick --> (_ => tabActive.set(t._2))
          )
        )
      ),
      div(
        cls := "tab-frame",
        tabs
          .map(_._2)
          .zipWithIndex
          .map(t =>
            div(
              cls := "tab-content",
              cls <-- tabActive.signal.map(a => if (a == t._2) "tab-visible" else "tab-hidden"),
              t._1.render()
            )
          )
      )
    )

}
