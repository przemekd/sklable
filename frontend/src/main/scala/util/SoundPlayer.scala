package util

import scala.scalajs.js
import org.scalajs.dom
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal
class Audio(url: String) extends js.Object {
  def play(): Unit = js.native
}

object SoundPlayer {
  private lazy val resourcesPrefix = s"${dom.document.location.protocol}//${dom.document.location.host}"

  def playNotificationSound() = playSound(s"${resourcesPrefix}/notification.mp3")

  private def playSound(url: String): Unit = {
    val audio = new Audio(url);
    audio.play();
  }
}
