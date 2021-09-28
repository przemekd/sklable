package shared

import shared.Board._
import wvlet.log.LogSupport

import scala.annotation.tailrec

object Sklable extends LogSupport {
  type Move = List[(Tile, Position)]
  type Word = List[(Tile, Position)]

  def isValidMove(previousMoves: List[Move], move: Move): Boolean =
    if (previousMoves.isEmpty) // First move
      if (move.length < 2) false
      else if (!move.exists { case (_, position) => position == Position(Rows.H, 8) }) false
      else areAllTilesNextToEachOther(previousMoves, move)
    else if (move.isEmpty) false
    else if (move.length == 1) isTileNextTo(previousMoves, move.head)
    else if (areAllTilesNextToEachOther(previousMoves, move) && move.exists(m => isTileNextTo(previousMoves, m))) true
    else false

  def calculatePoints(previousMoves: List[Move], move: Move): Int = {
    val wordsAndScores = getWordsAndScores(previousMoves, move)
    val log            = wordsAndScores.map { case (word, score) => s"${(word: RichWord).toString} [$score]" }.mkString(", ")
    debug(s"Words: $log")
    wordsAndScores.map { case (_, score) => score }.sum
  }

  def getWordsAndScores(previousMoves: List[Move], move: Move): List[(Word, Int)] =
    if (isValidMove(previousMoves, move)) {
      val moveWithLS         = applyLetterScoreMultiplier(move)
      val mainWordMultiplier = getWordScoreMultiplier(move)
      if (move.length == 1) {
        val vertical   = getVerticalWord(previousMoves, moveWithLS.head)
        val horizontal = getHorizontalWord(previousMoves, moveWithLS.head)

        List((horizontal, horizontal.score(mainWordMultiplier)), (vertical, vertical.score(mainWordMultiplier)))
          .filterNot(_._2 == 0)
      } else {
        val bonus = if (move.length == 7) 50 else 0

        if (isMoveHorizontal(move)) {
          val mainWord         = getVerticalWord(previousMoves :+ moveWithLS, moveWithLS.head)
          val mainWordAndScore = (mainWord, mainWord.score(mainWordMultiplier) + bonus)

          val horizontalList = moveWithLS
            .map { t =>
              val word = getHorizontalWord((List(t): Move) +: previousMoves, t)
              (word, word.score(getWordScoreMultiplier(List(t))))
            }
            .filterNot(_._2 == 0)
          List(mainWordAndScore) ++ horizontalList
        } else {
          val mainWord         = getHorizontalWord(previousMoves :+ moveWithLS, moveWithLS.head)
          val mainWordAndScore = (mainWord, mainWord.score(mainWordMultiplier) + bonus)
          val verticalList = moveWithLS
            .map { t =>
              val word = getVerticalWord((List(t): Move) +: previousMoves, t)
              (word, word.score(getWordScoreMultiplier(List(t))))
            }
            .filterNot(_._2 == 0)
          List(mainWordAndScore) ++ verticalList
        }
      }
    } else
      List.empty

  implicit class RichWord(word: List[(Tile, Position)]) {
    override def toString: String =
      word
        .map { t =>
          val c = t._1.char.trim
          if (c.isEmpty) "_" else c
        }
        .mkString(sep = "")

    def score(multiplier: Int = 1): Int =
      if (word.length < 2) 0 else word.map(t => t._1.value).sum * multiplier
  }

  def getVerticalWord(implicit moves: List[Move], tile: (Tile, Position)) =
    getWordSuffix(_.right)(getWordPrefix(_.left)(List(tile)))

  def getHorizontalWord(implicit moves: List[Move], tile: (Tile, Position)) =
    getWordSuffix(_.down)(getWordPrefix(_.up)(List(tile)))

  @tailrec
  private def getWordPrefix(
      findPrevious: Position => Option[Position]
  )(word: List[(Tile, Position)])(implicit moves: List[Move]): List[(Tile, Position)] =
    word match {
      case Nil => List.empty
      case ::(head, _) =>
        moves.flatten.find(p => findPrevious(head._2).contains(p._2)) match {
          case Some(value) => getWordPrefix(findPrevious)(value :: word)
          case None        => word
        }
    }

  @tailrec
  private def getWordSuffix(
      findNext: Position => Option[Position]
  )(word: List[(Tile, Position)])(implicit moves: List[Move]): List[(Tile, Position)] =
    word match {
      case Nil => List.empty
      case _ :+ end =>
        moves.flatten.find(p => findNext(end._2).contains(p._2)) match {
          case Some(value) => getWordSuffix(findNext)(word :+ value)
          case None        => word
        }
    }

  def getWordScoreMultiplier(move: Move): Int =
    move
      .map {
        case (_, position) =>
          if (Board.DoubleWordScoreSquares.contains(position)) 2
          else if (Board.TripleWordScoreSquares.contains(position)) 3
          else 1
      }.product

  def applyLetterScoreMultiplier(move: Move): Move =
    move.map {
      case (tile, position) =>
        tile match {
          case t: RegularTile =>
            if (Board.DoubleLetterScoreSquares.contains(position))
              (t.copy(value = t.value * 2), position)
            else if (Board.TripleLetterScoreSquares.contains(position))
              (t.copy(value = t.value * 3), position)
            else (t, position)
          case b: Blank => (b, position)
        }
    }

  def isTileNextTo(previousMoves: List[Move], t: (Tile, Position)) = {
    val positions      = previousMoves.flatMap(move => move.map(_._2))
    val validPositions = List(t._2.up, t._2.down, t._2.left, t._2.right).flatten
    positions.exists(p => validPositions.contains(p))
  }

  def areAllTilesNextToEachOther(previousMoves: List[Move], tiles: List[(Tile, Position)]): Boolean =
    if (isMoveHorizontal(tiles)) {
      val moveColumns = tiles.map(t => t._2.column).sorted
      val existingColumns = previousMoves.flatten
        .filter(t => t._2.row == tiles.head._2.row)
        .map(_._2.column)
        .filter(c => c > moveColumns.head && c < moveColumns.last)
      (moveColumns ++ existingColumns).sorted.sliding(2).forall {
        case x :: y :: Nil => x == y - 1
        case _             => true
      }
    } else if (isMoveVertical(tiles)) {
      val moveRows = tiles.map(t => t._2.row.index).sorted
      val existingRows = previousMoves.flatten
        .filter(t => t._2.column == tiles.head._2.column)
        .map(_._2.row.index)
        .filter(r => r > moveRows.head && r < moveRows.last)
      (moveRows ++ existingRows).sorted.sliding(2).forall {
        case x :: y :: Nil => x == y - 1
        case _             => true
      }
    } else false

  private def isMoveVertical(move: Move): Boolean =
    if (move.length < 2) false
    else {
      val columns = move.map(el => el._2.column)
      columns.forall(_ == columns.head)
    }

  private def isMoveHorizontal(move: Move): Boolean =
    if (move.length < 2) false
    else {
      val rows = move.map(el => el._2.row)
      rows.forall(_ == rows.head)
    }
}
