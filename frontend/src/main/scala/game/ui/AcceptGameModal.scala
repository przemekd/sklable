package game.ui

import com.raquo.laminar.api.L._
import org.scalajs.dom

import ru.makkarpov.scalingua.I18n.t
import services.I18nService
import shared.GameState
import shared.AboutToBeStarted
import services.PlayerService
import scala.scalajs.js
import scala.concurrent.duration._

class AcceptGameModal(gameStateSignal: Signal[GameState]) {

  private def formatTimer(seconds: Int) = {
    val mm = f"${seconds / 60}%02d"
    val ss = f"${seconds % 20}%02d"
    s"$mm:$ss"
  }

  def render(acceptFn: () => Unit) = {
    implicit val m                                  = locales.Languages
    implicit val languageId                         = I18nService.getLanguage()
    val secondsLeft                                 = Var[Int](0)
    var handle: Option[js.timers.SetIntervalHandle] = None

    val clickObserver = Observer[dom.MouseEvent](onNext = _ => acceptFn())

    div(
      cls := "overlay-msg",
      cls <-- gameStateSignal.map {
            case AboutToBeStarted(_, _, _) => ""
            case _                         => "overlay-hidden"
          },
      child <-- gameStateSignal.map {
            case AboutToBeStarted(_, needsToAccept, waitFor) =>
              val decreaseTimer: () => Unit = () => { secondsLeft.update((w) => w - 1) }
              secondsLeft.set(waitFor)
              if (needsToAccept.contains(PlayerService.getLocalPlayer().now().id.toString())) {
                handle = Some(js.timers.setInterval(1.second)(decreaseTimer()))
                div(
                  cls := "overlay-msg-picker",
                  child <-- (languageId.map { lang =>
                        implicit val id = lang; div(t("Game start?"))
                      }),
                  div(
                    child <-- secondsLeft.signal.map(s => com.raquo.laminar.api.L.span(s"(${formatTimer(s)})"))
                  ),
                  div(
                    button(
                      "ok",
                      onClick --> clickObserver
                    )
                  )
                )
              } else {
                handle = Some(js.timers.setInterval(1.second)(decreaseTimer()))
                div(
                  cls := "overlay-msg-picker",
                  child <-- (languageId.map { lang =>
                        implicit val id = lang; com.raquo.laminar.api.L.span(t("Waiting for other players to accept."))
                      }),
                  div(
                    child <-- secondsLeft.signal.map(s => com.raquo.laminar.api.L.span(s"(${formatTimer(s)})"))
                  )
                )
              }
            case _ =>
              handle.foreach(js.timers.clearInterval)
              div()
          }
    )
  }
}
