package util

sealed trait GameState

case object PlayerMove extends GameState
final case class WaitingForBlank(tileId: Int) extends GameState
final case class BlankProposed(tileId: Int, c: String) extends GameState
