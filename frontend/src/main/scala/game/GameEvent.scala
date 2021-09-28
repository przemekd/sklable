package game

import shared.Tile

sealed trait GameEvent

case object Observe                                            extends GameEvent
case object PlayerTurn                                         extends GameEvent
case object AcceptMove                                         extends GameEvent
case object Pass                                               extends GameEvent
final case class ProposeExchangeTiles(tiles: List[Tile])       extends GameEvent
final case class ExchangeTiles(tileIds: List[Tile])            extends GameEvent
case object WaitingForChallenge                                extends GameEvent
final case class GetBlankChar(tileId: Int)                     extends GameEvent
final case class ProposeBlankChar(tileId: Int, string: String) extends GameEvent
