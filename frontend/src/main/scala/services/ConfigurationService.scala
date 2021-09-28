package services

import org.scalajs.dom
import ru.makkarpov.scalingua.LanguageId
import scala.util.{ Failure, Success, Try }

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("navigator")
object navigator extends js.Object {
  def languages: js.Array[String] = js.native
}

object ConfigurationService {
  private val LANGUAGE_KEY = "_sklable_language"
  private val GAME_DURATION_KEY = "_sklable_game_duration"
  private val GAME_LANGUAGE_KEY = "_sklable_game_lang"

  def setLanguage(languageId: LanguageId) =
    dom.window.localStorage.setItem(
      LANGUAGE_KEY,
      languageId.toString
    )

  def getPreferredLanguages: List[LanguageId] = {
    val id   = dom.window.localStorage.getItem(LANGUAGE_KEY)
    val lang = Try(LanguageId(id))

    lang match {
      case Failure(_) =>
        navigator.languages.flatMap((l) => Try(LanguageId(l)).toOption).toList
      case Success(value) => List(value)
    }

  }

}
