package game.ui

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement

class TopBar(userSettingsLink: ReactiveHtmlElement[org.scalajs.dom.html.Div]) {
  val localePicker = new LocalesPicker()

  def render() =
    div(
      cls := "top-bar",
      div(
        cls := "top-bar-inner",
        div(
          cls := "top-bar-right",
          userSettingsLink,
          div(localePicker.render()),
//          div(a("X", onClick --> { _ => println("CLICK!") }))
        )
      )

    )

}
