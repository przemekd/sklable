package game.ui

import wvlet.log.LogSupport
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Paragraph

import scala.scalajs.js.Date

case class GameInfoMessage(id: Long, date: Date, text: String) {
  def render(): ReactiveHtmlElement[Paragraph] =
    p(
      idAttr := s"gim-$id",
      span(
        cls := "game-info-ts",
        f"[${date.getHours().toInt}%02d:${date.getMinutes().toInt}%02d] "
      ),
      text,
      onMountCallback(ctx => ctx.thisNode.ref.scrollIntoView())
    )
}

class GameInfoTab(private val bufferSize: Int) extends GameTab with LogSupport {
  class IdGenerator {
    private var id = 0L

    def getId: Long = {
      this.id += 1
      id
    }
  }

  private val idGenerator = new IdGenerator()

  private val messages: Var[List[GameInfoMessage]] = Var(List.empty[GameInfoMessage])

  private val messagesStream = messages.signal.split(_.id) { case (id, initialMsg, msgStream) => initialMsg.render() }

  def addMessage(msg: String): Unit =
    messages.update(l => l.takeRight(bufferSize) :+ GameInfoMessage(idGenerator.getId, new Date(), msg))

  def render(): Div = {
    val onEnterPress: EventProcessor[org.scalajs.dom.KeyboardEvent, org.scalajs.dom.KeyboardEvent] =
      onKeyPress.filter(_.keyCode == org.scalajs.dom.ext.KeyCode.Enter)

    div(
      div(
        cls := "game-updates",
        children <-- messagesStream
      ),
      div(
        cls := "game-chat-input",
        input(
          typ := "text",
          inContext(thisNode =>
            onEnterPress --> (ev => {
                  println(thisNode.ref.textContent)
                  thisNode.ref.value = ""
                })
          )
        )
      )
    )

  }

}
