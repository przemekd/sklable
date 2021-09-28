package game.ui

import wvlet.log.LogSupport
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import locales.Languages
import org.scalajs.dom.raw.HTMLSelectElement
import ru.makkarpov.scalingua.I18n._
import ru.makkarpov.scalingua.LanguageId
import shared.{GameConfig, SupportedLanguage}

class GameSettings(
    private val gameConfig: Var[GameConfig],
    private val canConfig: Signal[Boolean],
    private val updateConfig: GameConfig => Unit,
    private val langId: Signal[LanguageId]
) extends LogSupport
    with GameTab {
  implicit val languages = Languages

  def render(): Div =
    div(
      cls := "game-settings",
      child <-- langId.map(implicit id => t("Game language") + ": "),
      span(
        select(
          SupportedLanguage.values.map(l =>
            option(value := l.languageCode, l.language, selected <-- gameConfig.signal.map(_.language == l))
          ),
          onChange --> (ev => {
                val target = ev.target.asInstanceOf[HTMLSelectElement]
                val picked = target.options.map(_.value)(target.selectedIndex)
                val lang   = SupportedLanguage.values.find(_.languageCode == picked).getOrElse(gameConfig.now().language)
                updateConfig(GameConfig(gameConfig.now().minutes, lang))
              }),
          disabled <-- canConfig.map(!_)
        )
      ),
      br(),
      br(),
      child <-- langId.map(implicit id => t("Game duration") + ": "),
      span(
        select(
          option(value := "3", "3 min", selected <-- gameConfig.signal.map(_.minutes == 3)),
          option(value := "5", "5 min", selected <-- gameConfig.signal.map(_.minutes == 5)),
          option(value := "7", "7 min", selected <-- gameConfig.signal.map(_.minutes == 7)),
          option(value := "10", "10 min", selected <-- gameConfig.signal.map(_.minutes == 10)),
          disabled <-- canConfig.map(!_),
          onChange --> (ev => {
                val target = ev.target.asInstanceOf[HTMLSelectElement]
                val picked = target.options.map(_.value)(target.selectedIndex)
                if (picked.trim.nonEmpty)
                  updateConfig(GameConfig(picked.toInt, gameConfig.now().language))
              })
        )
      )
    )

}
