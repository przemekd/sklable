package services

import com.raquo.airstream.state.Var
import ru.makkarpov.scalingua.LanguageId

object I18nService {
  private val preferredLangs = ConfigurationService.getPreferredLanguages
  private val languageId = Var[LanguageId](getClientLang)

  def getLanguageId() = languageId

  def getLanguage() = languageId.signal

  def setLanguageId(languageId: LanguageId) = {
    ConfigurationService.setLanguage(languageId)
    this.languageId.set(languageId)
  }

  def supportedLanguages = locales.Languages.definedLanguages.toSeq :+ LanguageId("en-EN")

  private def getClientLang: LanguageId = {
    val maybeLanguage = preferredLangs.find((el) => {
      supportedLanguages.map(_.language.toLowerCase()).contains(el.language.toLowerCase())
    })

    maybeLanguage match {
      case Some(value) => value
      case None => LanguageId("en-EN")
    }
  }
}
