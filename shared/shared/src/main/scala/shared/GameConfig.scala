package shared

final case class GameConfig(minutes: Int, language: SupportedLanguage)

object GameConfig {
    def apply(): GameConfig = GameConfig(5, language = SupportedLanguage.PL)
}