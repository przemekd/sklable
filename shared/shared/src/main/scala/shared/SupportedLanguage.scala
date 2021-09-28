package shared

sealed abstract class SupportedLanguage(val languageCode: String, val language: String)

object SupportedLanguage {
  val values = List(EN, ES, PL)

  case object EN extends SupportedLanguage("EN", "english")
  case object ES extends SupportedLanguage("ES", "español")
  case object PL extends SupportedLanguage("PL", "polski")
}
