package game

import com.raquo.airstream.core.Transaction
import com.raquo.laminar.api.L._
import shared.Tile
import util._
import wvlet.log.LogSupport

import scala.annotation.tailrec

class Rack(
    private val draggedTile: Observer[Option[DraggedTile]],
    private val draggedEvents: EventBus[DraggableEvent],
    private val gameInteractions: EventBus[GameEvent]
) extends LogSupport {
  private val rack: Var[List[Option[Tile]]] = Var(List.range(1, 8).map(_ => None))

  val dropZoneClass = "rack"

  private def getDraggable =
    (tile: Tile, position: String) =>
      game.DraggableTile(
        tile,
        draggedTile,
        draggedEvents.writer,
        (dt: DraggableTile) => draggedEvents.writer.onNext(DropInto(dropZoneClass, position, dt))
      )

  def proposeExchange(proposed: List[Tile], tile: Tile, remove: Boolean = false) =
    if (!remove)
      gameInteractions.writer.onNext(ProposeExchangeTiles(proposed.appended(tile)))
    else
      gameInteractions.writer.onNext(ProposeExchangeTiles(proposed.filterNot(_.id == tile.id)))

  def render(): Div = {
    val signal = (pos: Int) =>
      rack.signal
        .combineWith(gameInteractions.events.toWeakSignal)
        .map {
          case (maybeTile, maybeEvent) =>
            val tryTile = maybeTile.lift(pos - 1).getOrElse(None)
            tryTile match {
              case Some(tile) =>
                maybeEvent match {
                  case Some(event) =>
                    event match {
                      case ProposeExchangeTiles(tiles) =>
                        if (tiles.contains(tile))
                          Some(ExchangeTile(tile, selected = true, s => proposeExchange(tiles, tile, !s)))
                        else Some(ExchangeTile(tile, selected = false, s => proposeExchange(tiles, tile, !s)))
                      case _ => Some(getDraggable(tile, pos.toString))
                    }
                  case None => Some(getDraggable(tile, pos.toString))
                }
              case None => None
            }
        }

    val spaces = List.range(1, 8).map(pos => RackSpace(pos, signal(pos)))

    div(
      cls := "rack",
      dataAttr("game-interactions") <-- gameInteractions.events.toWeakSignal.map {
            case Some(value) =>
              value match {
                case ProposeExchangeTiles(_) => "exchange"
                case _                       => "non-exchange"
              }
            case None => "none"
          },
      dataAttr("over") <-- draggedEvents.events.map {
            case OutOfDropzone => "none"
            case OverDropzone(dropZone, position) =>
              if (dropZone == dropZoneClass) {
                makeSpace(position.toInt)
                position
              } else
                "none"
            case DropInto(dropZone, position, tile) =>
              if (dropZone == dropZoneClass) {
                // debug(s"Dropping into $position, start")
                makeSpace(position.toInt)
                new Transaction(_ =>
                  rack.update { old =>
                    // debug(s"Dropping into $position, old rack ${rackToStr(old)}")
                    val newRack = old.updated(position.toInt - 1, Some(tile.tile))
                    // debug(s"Dropping into $position, old rack ${rackToStr(newRack)}")
                    newRack
                  }
                )
                // debug(s"Dropping into $position, end")
                position
              } else
                "none"
            case DragFromRackInto(dropzone, position) =>
              rack.now().flatten.headOption match {
                case Some(tile) =>
                  removeTile(tile.id)
                  draggedEvents.writer.onNext(DropInto(dropzone, position, getDraggable(tile, position)))
                  "none"
                case None => "none"
              }
          },
      spaces
    )
  }

  def makeSpace(position: Int) =
    rack.update { old =>
      // debug(s"making space in $position, old rack: ${rackToStr(old)}")
      if (old(position - 1).isEmpty)
        // debug(s"making space in $position, new rack: ${rackToStr(old)}")
        old
      else {
        @tailrec
        def getClosestNone(pos: Int, radius: Int): Int = {
          val leftIndex  = pos - radius
          val rightIndex = pos + radius
          if (leftIndex >= 0 && old(leftIndex).isEmpty) return leftIndex
          if (rightIndex < old.length && old(rightIndex).isEmpty) rightIndex else getClosestNone(pos, radius + 1)
        }

        val noneAt = getClosestNone(position - 1, 1)

        val (front, back) = if (noneAt < position - 1) old.splitAt(position) else old.splitAt(position - 1)
        val idx           = if (noneAt > position - 1) noneAt + 1 else noneAt
        val withSpace     = (front ++ List(None) ++ back).zipWithIndex

        val newRack = withSpace.filterNot(el => el._2 == idx).map(_._1)
        // debug(s"making space in $position, new rack: ${rackToStr(newRack)}")
        newRack
      }
    }

  def maybeTile(maybeTileSignal: Signal[Option[TileDiv]]): Signal[HtmlElement] =
    maybeTileSignal.map {
      case Some(tile) => tile.render()
      case None       => div()
    }

  def getFreeSpace(): Int = rack.now().filter(_.isEmpty).count(_ => true)

  def addTiles(tiles: List[Tile]) = {
    val rackIds = rack.now().flatten.map(tile => tile.id)

    rack.update { old =>
      // debug(s"Adding tiles ${tiles.map(_.char).mkString("")} to rack")

      @tailrec
      def add(maybeTiles: List[Option[Tile]], tiles: List[Tile]): List[Option[Tile]] =
        tiles match {
          case head :: tail => add(maybeTiles.updated(maybeTiles.indexWhere(_.isEmpty), Some(head)), tail)
          case Nil          => maybeTiles
        }

      val newTiles = tiles.filterNot(tile => rackIds.contains(tile.id))
      add(old, newTiles)
    }
  }

  def putTiles(tiles: List[Tile]) =
    rack.update { old =>
      @tailrec
      def put(maybeTiles: List[Option[Tile]], tiles: List[Tile]): List[Option[Tile]] =
        tiles match {
          case head :: tail =>
            if (maybeTiles.contains(Some(head)))
              put(maybeTiles, tail)
            else
              put(maybeTiles.updated(maybeTiles.indexWhere(_.isEmpty), Some(head)), tail)
          case Nil => maybeTiles
        }

      val cleanOld = old.map {
        case Some(tile) => if (tiles.contains(tile)) Some(tile) else None
        case None       => None
      }

      put(cleanOld, tiles)
    }

  def addTile(tile: Tile) =
    // debug(s"Adding tile $tile")
    // debug(s"${rack.now().indexWhere(_.isEmpty)}")
    rack.update(old => old.updated(old.indexWhere(_.isEmpty), Some(tile)))

  def removeTile(tileId: Int) = {
    trace(s"Removing tile $tileId")
    rack.update(_.map { 
        case Some(t) => if (t.id == tileId) None else Some(t)
        case None    => None
      })
  }

  def removeTiles(tiles: List[Tile]) =
    rack.update(_.map {
      case Some(t) => if (tiles.map(_.id).contains(t.id)) None else Some(t)
      case None    => None
    })

  def exchangeTiles(oldTiles: List[Tile], newTiles: List[Tile]) = {
    rack.update { old =>
      old.map {
          case None        => None
          case Some(value) => if (oldTiles.contains(value)) None else Some(value)
        }
    }
    new Transaction(_ => addTiles(newTiles))
  }

  def RackSpace(pos: Int, tile: Signal[Option[TileDiv]]): Div =
    div(
      cls := "rackSpace",
      cls := "droppable",
      dataAttr("dropzone-class") := dropZoneClass,
      dataAttr("dropzone-position") := pos.toString,
      child <-- maybeTile(tile)
    )
}
