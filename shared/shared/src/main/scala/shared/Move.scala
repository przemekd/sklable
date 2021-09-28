package shared

sealed trait Move

case object Pass                                                   extends Move
case object InvalidMove                                            extends Move
case object TimedOut                                               extends Move
final case class ExchangeTiles(tiles: List[Tile])                  extends Move
final case class PlayTiles(tiles: List[(Tile, Board.Position)])    extends Move
final case class ProposedPlay(tiles: List[(Tile, Board.Position)]) extends Move
