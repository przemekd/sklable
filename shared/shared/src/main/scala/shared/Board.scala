package shared

object Board {
  type Column = Int

  final case class Position(row: Rows.Row, column: Column) {
    def up: Option[Position]    = Rows.rows.find(p => p.index == row.index - 1).map(Position(_, column))
    def down: Option[Position]  = Rows.rows.find(p => p.index == row.index + 1).map(Position(_, column))
    def left: Option[Position]  = if (column == 1) None else Some(Position(row, column - 1))
    def right: Option[Position] = if (column == 15) None else Some(Position(row, column + 1))
  }

  object Rows {

    sealed abstract class Row(val index: Int, val name: Char)

    case object A extends Row(1, 'A')
    case object B extends Row(2, 'B')
    case object C extends Row(3, 'C')
    case object D extends Row(4, 'D')
    case object E extends Row(5, 'E')
    case object F extends Row(6, 'F')
    case object G extends Row(7, 'G')
    case object H extends Row(8, 'H')
    case object I extends Row(9, 'I')
    case object J extends Row(10, 'J')
    case object K extends Row(11, 'K')
    case object L extends Row(12, 'L')
    case object M extends Row(13, 'M')
    case object N extends Row(14, 'N')
    case object O extends Row(15, 'O')

    val rows = List(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
  }

  import scala.language.implicitConversions

  implicit def string2Position(string: String) = {
    val row    = Rows.rows.find(row => row.name == string.charAt(0)).get
    val column = string.tail.toInt
    Position(row, column)
  }

  implicit def position2String(position: Position) = position.row.name.toString + position.column.toString

  val TripleWordScoreSquares: List[Position] = List("A1", "A8", "A15", "H1", "H15", "O1", "O8", "O15")
  val DoubleWordScoreSquares: List[Position] =
    List("B2", "B14", "C3", "C13", "D4", "D12", "E5", "E11", "K5", "K11", "L4", "L12", "M3", "M13", "N2", "N14", "H8")
  val TripleLetterScoreSquares: List[Position] =
    List("B6", "B10", "F2", "F6", "F10", "F14", "J2", "J6", "J10", "J14", "N6", "N10")
  val DoubleLetterScoreSquares: List[Position] = List(
    "A4",
    "A12",
    "C7",
    "C9",
    "D1",
    "D8",
    "D15",
    "G3",
    "G7",
    "G9",
    "G13",
    "H4",
    "H12",
    "I3",
    "I7",
    "I9",
    "I13",
    "L1",
    "L8",
    "L15",
    "M7",
    "M9",
    "O4",
    "O12"
  )
}
