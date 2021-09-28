package util

import game.DraggableTile

sealed trait DraggableEvent

case object OutOfDropzone                                                    extends DraggableEvent
final case class OverDropzone(dropzone: String, position: String)                  extends DraggableEvent
final case class DropInto(dropzone: String, position: String, tile: DraggableTile) extends DraggableEvent
final case class DragFromRackInto(dropzone: String, position: String)              extends DraggableEvent
