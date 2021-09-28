package game

import com.raquo.laminar.api.L._
// import com.raquo.laminar.modifiers.EventPropBinder
import com.raquo.laminar.modifiers.EventListener
import org.scalajs.dom
import shared.Board.Position
import shared.Sklable.Move
import shared.{ SupportedLanguage, Tile }
import util.{ DragFromRackInto, DraggableEvent, DropInto, OverDropzone }
import wvlet.log.LogSupport
import services.I18nService
import ru.makkarpov.scalingua.I18n.t
import game.ui.MoveUI
import game.ui.BlinkingMove
import scala.scalajs.js

class Board(
    private val draggedTile: Observer[Option[DraggedTile]],
    private val draggedEvents: EventBus[DraggableEvent],
    private val gameInteractions: EventBus[GameEvent],
    private val enabledLanguage: Signal[SupportedLanguage]
) extends LogSupport {

  private val dropZoneClass = "board"

  private val previousMoves: Var[List[MoveUI]] = Var(List.empty)
  private val shouldBlink: Var[Boolean]        = Var(true)
  private val move: Var[Move]                  = Var(List.empty[(Tile, Position)])

  private val allMoves            = previousMoves.signal.combineWith(move.signal)
  private implicit val m          = locales.Languages
  private implicit val languageId = I18nService.getLanguage()

  private var timer: Option[js.timers.SetTimeoutHandle] = None

  def render(): Div = {
    val allPositions = for {
      col <- 1 to 15
      row <- shared.Board.Rows.rows
    } yield Position(row, col)

    val spaces = allPositions.map { pos =>
      val signal = allMoves.combineWith(gameInteractions.events.toWeakSignal).combineWith(enabledLanguage) map {
            case (previousMoves, move, maybeGameInteraction, lang) =>
              val (previousSolid, previousBlinking) = previousMoves.partition(m =>
                m match {
                  case BlinkingMove(_) => false
                  case _               => true
                }
              )
              val maybeTileDiv: Option[TileDiv] = previousSolid
                .map(_.move)
                .flatten
                .find(_._2 == pos)
                .map(tile => game.SolidTile(tile._1))
                .orElse({
                  val blinking = previousBlinking.map(_.move).flatten
                  blinking.find(_._2 == pos) match {
                    case Some(tile) => Some(game.BlinkingTile(tile._1, shouldBlink.signal))
                    case None       => None
                  }
                })
                .orElse({
                  move.find(_._2 == pos).map {
                    tile =>
                      def getDraggable =
                        game.DraggableTile(
                          tile._1,
                          draggedTile,
                          draggedEvents.writer,
                          (tile: DraggableTile) => draggedEvents.writer.onNext(DropInto(dropZoneClass, pos, tile))
                        )

                      maybeGameInteraction match {
                        case Some(value) =>
                          value match {
                            case WaitingForChallenge => game.SolidTile(tile._1)
                            case PlayerTurn          => getDraggable
                            case GetBlankChar(tileId) =>
                              if (tile._1.id == tileId)
                                BlankCarousel(
                                  tile._1,
                                  shared.TileBag.getTileBagByLang(lang).getChars,
                                  "",
                                  (c: String) => {
                                    gameInteractions.writer.onNext(ProposeBlankChar(tile._1.id, c))
                                    debug(s"Changed to $c")
                                  }
                                )
                              else game.SolidTile(tile._1)
                            case ProposeBlankChar(tileId, c) =>
                              if (tile._1.id == tileId)
                                BlankCarousel(
                                  tile._1,
                                  shared.TileBag.getTileBagByLang(lang).getChars,
                                  c,
                                  (c: String) => {
                                    gameInteractions.writer.onNext(ProposeBlankChar(tile._1.id, c))
                                    debug(s"Changed to $c")
                                  }
                                )
                              else game.SolidTile(tile._1)
                            case _ => game.SolidTile(tile._1)
                          }
                        case None => getDraggable
                      }
                  }
                })
              maybeTileDiv
          }

      TileSpace(pos, signal)
    }

    div(
      cls := "board",
      dataAttr("over") <-- draggedEvents.events.map {
            case OverDropzone(dropZone, position) =>
              if (dropZone == dropZoneClass) {
                timer.map(js.timers.clearTimeout(_))
                shouldBlink.set(false)
                position
              } else "none"
            case DropInto(dropZone, position, tile) =>
              if (dropZone == dropZoneClass) {
                debug(s"Dropping into $position")
                timer.map(js.timers.clearTimeout(_))
                shouldBlink.set(false)
                move.update(_ ++ List((tile.tile, position)))
                position
              } else
                "none"
            case _ => ""
          },
      spaces,
    )
  }

  def getMove(): Var[Move] = move

  def getPreviousMoves(): Var[List[MoveUI]] = previousMoves

  def maybeTile(maybeTileSignal: Signal[Option[TileDiv]], pos: String): Signal[HtmlElement] =
    maybeTileSignal.map {
      case Some(tile) => tile.render()
      case None       => boardSpaceType(pos)
    }

  def boardSpaceType(pos: String): Div = {
    def eachWordInNewLine(s: String): Span = {
      val arr = s.split(" ")
      val el  = span()
      arr.foreach { s =>
        el.amend(s)
        el.amend(br())
      }
      el
    }

    pos match {
      case pos if shared.Board.TripleWordScoreSquares.contains(pos: Position) =>
        div(
          cls := "boardSpaceType",
          cls := "boardSpaceTripleWordScore",
          child <-- languageId.map(implicit id => t("TRIPLE WORD SCORE")).map(eachWordInNewLine)
        )
      case pos if shared.Board.DoubleWordScoreSquares.contains(pos: Position) =>
        if (pos == "H8")
          div(cls := "boardSpaceType", cls := "boardSpaceDoubleWordScore", fontSize := "25px", "★")
        else
          div(
            cls := "boardSpaceType",
            cls := "boardSpaceDoubleWordScore",
            child <-- languageId.map(implicit id => t("DOUBLE WORD SCORE")).map(eachWordInNewLine)
          )
      case pos if shared.Board.TripleLetterScoreSquares.contains(pos: Position) =>
        div(
          cls := "boardSpaceType",
          cls := "boardSpaceTripleLetterScore",
          child <-- languageId.map(implicit id => t("TRIPLE LETTER SCORE")).map(eachWordInNewLine)
        )
      case pos if shared.Board.DoubleLetterScoreSquares.contains(pos: Position) =>
        div(
          cls := "boardSpaceType",
          cls := "boardSpaceDoubleLetterScore",
          child <-- languageId.map(implicit id => t("DOUBLE LETTER SCORE")).map(eachWordInNewLine)
        )
      case _ => div()
    }
  }

  def isValidMoveAdded(): Signal[Boolean] =
    previousMoves.signal.combineWith(move.signal).map { moves =>
      val previous = moves._1.map(m => m.move)
      shared.Sklable.isValidMove(previous, moves._2)
    }

  def removeTile(tileId: Int) = {
    debug(s"Removing tile $tileId")
    move.update(_.filter(t => t._1.id != tileId))
  }

  def updateMoves(moves: List[MoveUI]) = {
    shouldBlink.set(true);
    timer.map(js.timers.clearTimeout(_))
    timer = Some({
      js.timers.setTimeout(6000) {
        shouldBlink.set(false)
      }
    })
    previousMoves.writer.onNext(moves)
  }

  def updateMove(move: Move) = this.move.writer.onNext(move)

  def freeze() = {
    val m = move.now()
    move.update(_ => List.empty)
    shouldBlink.set(true);
    previousMoves.update(moves => moves ++ List(BlinkingMove(m)))
  }

  def TileSpace(position: String, tile: Signal[Option[TileDiv]]): Div = {
    val callback = (_: dom.PointerEvent) => draggedEvents.writer.onNext(DragFromRackInto(dropZoneClass, position))
    // val binder   = new EventPropBinder(onPointerDown, callback, useCapture = false)
    val binder                                    = new EventListener(onPointerDown, callback)
    var subscription: Option[DynamicSubscription] = None

    // FIXME: BLOCK clicking if game state doesn't allow it
    lazy val tileSpaceDiv: Div = div(
      cls := "boardSpace",
      cls <-- tile.combineWith(gameInteractions.events.startWith(PlayerTurn)).map {
            case (maybeTile, interaction) =>
              interaction match {
                case PlayerTurn =>
                  maybeTile match {
                    case Some(_) =>
                      subscription.foreach(_.kill())
                      subscription = None
                      // ReactiveElement.removeEventListener(tileSpaceDiv, binder)
                      ""
                    case None =>
                      subscription = Some(binder.bind(tileSpaceDiv))
                      // ReactiveElement.addEventListener(tileSpaceDiv, binder)
                      "droppable"
                  }
                case _ =>
                  subscription.foreach(_.kill())
                  subscription = None
                  // ReactiveElement.removeEventListener(tileSpaceDiv, binder)
                  ""
              }
          },
      dataAttr("dropzone-class") := "board",
      dataAttr("dropzone-position") := position,
      child <-- maybeTile(tile, position)
    )

    tileSpaceDiv
  }
}
