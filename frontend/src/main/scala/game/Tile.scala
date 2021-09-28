package game

import com.raquo.laminar.api.L._
import com.raquo.laminar.modifiers.EventListener
import org.scalajs.dom
import shared.Tile
import util.{ DOMUtils, DropInto }
import wvlet.log._
import scala.scalajs.js
import util.{ OutOfDropzone, OverDropzone }

@js.native
trait PointerElement extends dom.html.Div {
  def setPointerCapture(id: Double): this.type     = js.native
  def releasePointerCapture(id: Double): this.type = js.native
  def hasPointerCapture(id: Double): this.type     = js.native
}

object PointerElement {
  import scala.language.implicitConversions

  implicit def el2PointerElement(el: dom.html.Div): PointerElement =
    el.asInstanceOf[PointerElement]
}

import PointerElement._

sealed abstract class TileDiv {
  val tile: Tile

  def render(): Div
}

final case class SolidTile(tile: Tile) extends TileDiv {
  // https://javascript.info/mouse-drag-and-drop
  def render(): Div =
    div(
      cls := "tile",
      draggable := false,
      onDragStart.stopPropagation.preventDefault --> { _ => },
      dataAttr("tile-type") := "solid",
      div(
        cls := "tileCharacter",
        tile.char
      ),
      div(
        cls := "tileValue",
        if (tile.value != 0) tile.value.toString else ""
      )
    )
}

final case class BlinkingTile(tile: Tile, shouldBlink: Signal[Boolean]) extends TileDiv {
  def render(): Div =
    div(
      cls := "tile",
      cls <-- shouldBlink.map(b => if(b) "blinkingTile" else ""),
      draggable := false,
      onDragStart.stopPropagation.preventDefault --> { _ => },
      dataAttr("tile-type") := "solid",
      div(
        cls := "tileCharacter",
        tile.char
      ),
      div(
        cls := "tileValue",
        if (tile.value != 0) tile.value.toString else ""
      )
    )
}

final case class ExchangeTile(tile: Tile, selected: Boolean = false, onChange: Boolean => Unit) extends TileDiv {
  val s = Var[Boolean](selected)

  def render() =
    div(
      cls := "tile",
      draggable := false,
      onDragStart.stopPropagation.preventDefault --> { _ => },
      dataAttr("tile-type") := "exchange",
      div(
        cls := "tileCharacter",
        tile.char
      ),
      div(
        cls := "tileValue",
        if (tile.value != 0) tile.value.toString else ""
      ),
      cls <-- s.signal.map(c => if (c) "selected" else ""),
      inContext(thisNode =>
        onPointerDown --> (ev => {
              ev.stopPropagation()
              s.update(s => !s)
              onChange(s.now())
            })
      )
    )
}

final case class BlankCarousel(tile: Tile, chars: List[String], c: String = "", onChange: String => Unit)
    extends TileDiv {
  var i = Var[Int](chars.indexOf(c))

  def getNext() = i.update(i => if (i + 1 >= chars.length) 0 else i + 1)

  def getPrevious() = i.update(i => if (i - 1 < 0) chars.length - 1 else i - 1)

  // https://javascript.info/mouse-drag-and-drop
  def render() =
    div(
      cls := "tile",
      draggable := false,
      dataAttr("tile-type") := "blank-select",
      dataAttr("blank") <-- i.signal.map { i =>
            val c = chars.lift(i)
            c match {
              case Some(value) =>
                onChange(value)
                value
              case None => "not-set"
            }
          },
      div(
        cls := "tileCharacter",
        child <-- i.signal.map(i =>
              chars.lift(i) match {
                case Some(value) => span(value)
                case None        => span()
              }
            ),
        div(
          cls := "tileValue",
          if (tile.value != 0) tile.value.toString else ""
        ),
        div(
          cls := "arrow",
          cls := "arrow-left",
          onPointerDown --> (_ => getPrevious())
        ),
        div(
          cls := "arrow",
          cls := "arrow-right",
          onPointerDown --> (_ => getNext())
        )
      )
    )
}

final case class DraggedTile(
    draggableTile: DraggableTile,
    absoluteX: Double,
    absoluteY: Double,
    shiftX: Double,
    shiftY: Double,
    pointerId: Double
) extends TileDiv
    with LogSupport {
  val tile = draggableTile.tile

  def render() = {
    val tileDiv = div(
      cls := "tile",
      cls := "dragged",
      draggable := false,
      dataAttr("tile-type") := "dragged",
      zIndex := 999,
      left := (absoluteX - shiftX).toString + "px",
      top := (absoluteY - shiftY).toString + "px",
      onDragStart.stopPropagation.preventDefault --> { _ => },
      position := "absolute",
      inContext { thisNode =>
        onPointerMove.map {
          ev =>
            ev.stopPropagation()
            ev.preventDefault()
            // debug(s"HasPointer: ${thisNode.ref.hasPointerCapture(pointerId)}")
            // debug(thisNode.ref.setPointerCapture(pointerId))
            // debug(s"Dragging, pointerId[${ev.pointerId}]")
            val elementsBelow = DOMUtils.elementsFromPoint(ev.clientX, ev.clientY)
            val elemBelow     = elementsBelow.find(el => el != null && el.classList.contains("droppable"))
            thisNode.ref.style.left = (ev.pageX - this.shiftX).toString + "px"
            thisNode.ref.style.top = (ev.pageY - this.shiftY).toString + "px"

            elemBelow match {
              case Some(value) =>
                val dropZonePosition = value.getAttribute("data-dropzone-position")
                val dropZone         = value.getAttribute("data-dropzone-class")
                OverDropzone(dropZone, dropZonePosition)
              case None => OutOfDropzone
            }
        } --> draggableTile.draggableWriteBus
      },
      inContext { thisNode =>
        onPointerUp.map {
          ev =>
            ev.stopPropagation()
            thisNode.ref.releasePointerCapture(pointerId)
            // debug(s"Drag end, pointerId[${ev.pointerId}]")

            val elementsBelow = DOMUtils.elementsFromPoint(ev.clientX, ev.clientY)
            val elemBelow     = elementsBelow.find(el => el != null && el.classList.contains("droppable"))

            elemBelow match {
              case Some(value) =>
                val dropZonePosition = value.getAttribute("data-dropzone-position")
                val dropZone         = value.getAttribute("data-dropzone-class")
                draggableTile.draggableWriteBus.onNext(DropInto(dropZone, dropZonePosition, draggableTile))
              case None => draggableTile.noDropZoneFn(draggableTile)
            }

            None
        } --> draggableTile.draggedTile
      },
      onMountCallback { el =>
        // debug(s"Mounting, pointerId[$pointerId]")
        el.thisNode.ref.setPointerCapture(pointerId)
      },
      div(
        cls := "tileCharacter",
        tile.char
      ),
      div(
        cls := "tileValue",
        if (tile.value != 0) tile.value.toString else ""
      )
    )
    tileDiv
  }

}

final case class DraggableTile(
    tile: Tile,
    draggedTile: Observer[Option[DraggedTile]],
    draggableWriteBus: WriteBus[util.DraggableEvent],
    noDropZoneFn: DraggableTile => Unit
) extends TileDiv
    with LogSupport {

  // https://javascript.info/mouse-drag-and-drop
  // https://stackoverflow.com/questions/62807804/does-setpointercapture-works-only-for-pointerdown-event
  def render(): Div = {

    lazy val draggableTile: Div = div(
      cls := "tile",
      dataAttr("tile-type") := "draggable",
      draggable := false,
      onDragStart.stopPropagation.preventDefault --> { _ => },
      div(
        cls := "tileCharacter",
        tile.char
      ),
      div(
        cls := "tileValue",
        if (tile.value != 0) tile.value.toString else ""
      )
    )

    val callback = (ev: dom.PointerEvent) => {
      // debug(s"Drag start, pointerId[${ev.pointerId}]")
      ev.stopPropagation()
      val shiftX =
        ev.pageX - draggableTile.ref.getBoundingClientRect().left + dom.document.body.getBoundingClientRect().left
      val shiftY =
        ev.pageY - draggableTile.ref.getBoundingClientRect().top + dom.document.body.getBoundingClientRect().top

      draggedTile.onNext(
        Some(
          DraggedTile(
            DraggableTile(tile, draggedTile, draggableWriteBus, noDropZoneFn),
            ev.pageX,
            ev.pageY,
            shiftX,
            shiftY,
            ev.pointerId
          )
        )
      )
    }

    // val binder = new EventListener(onPointerDown, callback, useCapture = false)
    val binder = new EventListener(onPointerDown, callback)

    binder.bind(draggableTile)
    // ReactiveElement.addEventListener(draggableTile, binder)

    draggableTile
  }

}
