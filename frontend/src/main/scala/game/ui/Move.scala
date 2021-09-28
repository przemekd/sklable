package game.ui

import shared.Sklable.Move

sealed trait MoveUI {
    def move: Move
}

case class SolidMove(move: Move) extends MoveUI
case class BlinkingMove(move: Move) extends MoveUI