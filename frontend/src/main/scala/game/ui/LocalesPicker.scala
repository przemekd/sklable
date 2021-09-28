package game.ui

import com.raquo.laminar.api.L._
import org.scalajs.dom.raw.HTMLSelectElement
import ru.makkarpov.scalingua.LanguageId
import wvlet.log.LogSupport
import services.I18nService

class LocalesPicker() extends LogSupport {
  private def langIdToString(id: LanguageId): String = id.language.toLowerCase() match {
    case "pl" => "Polski"
    case "en" => "English"
    case "es" => "Español"
    case _ => "Unknown"
  }

  def render(): Div =
    div(
      img(src := "../langicon.png", height:= "12px"),
      "  ",
      select(
        I18nService.supportedLanguages.map { languageId =>
          option(
            value := languageId.language,
            this.langIdToString(languageId),
            selected <-- I18nService.getLanguage().map(_.language == languageId.language)
          )
        },
        onChange --> (ev => {
              val target = ev.target.asInstanceOf[HTMLSelectElement]
              val picked = target.options.map(_.value)(target.selectedIndex)
              I18nService.setLanguageId(LanguageId(picked, picked.toUpperCase))
            })
      )
    )
}
