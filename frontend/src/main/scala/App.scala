import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.document
import com.raquo.waypoint._
import game.ui.TopBar
import upickle.default._
import wvlet.log._
import services.{ I18nService, PlayerService }

import scala.util.Success
import scala.util.Failure
import ru.makkarpov.scalingua.I18n._

sealed trait Page

final case class RoomPage(roomId: String) extends Page
final case object UserPage                extends Page
final case object IndexPage               extends Page

object App extends LogSupport {
  implicit val m          = locales.Languages
  implicit val languageId = I18nService.getLanguage()

  Logger.setDefaultLogLevel(LogLevel.DEBUG)

  private val SPACE_KEYCODE = 32

  implicit val RoomPageRW: ReadWriter[RoomPage] = macroRW
  implicit val rw: ReadWriter[Page]             = macroRW

  private var redirectTo: Option[Page] = None

  val roomRoute = Route(
    encode = (room: RoomPage) => room.roomId,
    decode = (arg: String) => RoomPage(roomId = arg),
    pattern = root / "room" / segment[String] / endOfSegments
  )
  val userRoute  = Route.static(UserPage, root / "user" / endOfSegments)
  val indexRoute = Route.static(IndexPage, root / endOfSegments)

  val router = new Router[Page](
    routes = List(roomRoute, userRoute, indexRoute),
    // initialUrl = dom.document.location.href, // must be a valid LoginPage or UserPage url
    // origin = dom.document.location.origin.get,
    getPageTitle = _.toString,                     // mock page title (displayed in the browser tab next to favicon)
    serializePage = page => write(page)(rw),       // serialize page data for storage in History API log
    deserializePage = pageStr => read(pageStr)(rw) // deserialize the above
  )(
    $popStateEvent =
      windowEvents.onPopState, // this being a param lets Waypoint avoid an explicit dependency on Laminar
    owner = unsafeWindowOwner  // this router will live as long as the window
  )

  val localPlayer = PlayerService.getLocalPlayer()

  debug(s"Got player $localPlayer")

  import scala.concurrent.ExecutionContext.Implicits.global
  def renderPage(page: Page): Div =
    page match {
      case IndexPage =>
        val userSettingsLink = div(
          a(
            span("⚙"),
            onClick --> { _ =>
              router.pushState(UserPage)
            }
          )
        )
        val topBar = new TopBar(userSettingsLink)

        div(
          div(
            cls := "github-badge",
            a(
              img(
                loadingAttr := "lazy",
                width := "149",
                height := "149",
                src := "https://github.blog/wp-content/uploads/2008/12/forkme_left_darkblue_121621.png?resize=149%2C149",
                cls := "attachment-full size-full",
                alt := "Fork me on GitHub",
                dataAttr("recalc-dims") := "1"
              ),
              href := "https://github.com/przemekd/sklable"
            )
          ),
          topBar.render(),
          div(
            cls := "main-content",
            div(
              cls := "main-content-column",
              div(
                button(
                  child <-- languageId.map(implicit id => t("Create new game")),
                  onClick --> { _ =>
                    router.pushState(RoomPage(shared.Room.getRandomId()))
                  }
                )
              )
            )
          )
        )

      case RoomPage(roomId) =>
        val sg     = div(idAttr := "sg")
        val topBar = new TopBar(div())
        PlayerService
          .insertOrUpdateUser(localPlayer.now().name)
          .onComplete {
            case Failure(_) =>
              redirectTo = Some(RoomPage(roomId))
              router.pushState(UserPage)
            case Success(_) =>
              val game = new SklableGame(roomId)

              dom.window.onkeypress = { ev =>
                if (ev.keyCode == SPACE_KEYCODE || ev.key == " ")
                  game.nextRound()
                !(ev.target == document.body)
              }

              sg.amend(
                div(
                  topBar.render(),
                  game.Game()
                )
              )
          }
        sg

      case UserPage =>
        val nameBus = Var[String](localPlayer.now().name)
        val topBar  = new TopBar(div())

        div(
          topBar.render(),
          div(
            div(
              child <-- languageId.map(implicit id => t("Hello there fellow scrabble aficionado!")),
              child <-- languageId.map(implicit id => t("What's your name?"))
            ),
            div(
              input(
                name := "name",
                typ := "text",
                placeholder <-- languageId.map(implicit id => t("Your name")),
                inContext(thisNode => onInput.mapTo(thisNode.ref.value) --> nameBus.writer)
              ),
              div(
                button(
                  "ok",
                  disabled <-- nameBus.signal.map(name => !PlayerService.isUserNameValid(name)),
                  onClick --> (_ => {
                        println("Setting name as " + nameBus.now())
                        PlayerService
                          .insertOrUpdateUser(nameBus.now())
                          .onComplete {
                            case Failure(exception) =>
                            case Success(value) =>
                              redirectTo match {
                                case Some(value) =>
                                  redirectTo = None
                                  router.pushState(value)
                                case None => router.pushState(IndexPage)
                              }
                          }
                      })
                )
              ),
              div(
                color := "red",
                children <-- nameBus.signal.map(name => PlayerService.validateUserName(name).map(div(_)))
              )
            )
          )
        )
    }

  val app: Div = div(child <-- router.$currentPage.map(renderPage))

  def main(args: Array[String]): Unit =
    // If element exist render, if not wait until the DOM is loaded
    println("Starting the app.")
  if (dom.document.getElementById("app-container") != null)
    render(
      dom.document.getElementById("app-container"), // make sure you add such a container element to your HTML.
      app
    )
  else
    documentEvents.onDomContentLoaded.foreach { _ =>
      render(
        dom.document.getElementById("app-container"), // make sure you add such a container element to your HTML.
        app
      )
    }(unsafeWindowOwner)
}
