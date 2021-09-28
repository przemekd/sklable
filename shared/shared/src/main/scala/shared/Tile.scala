package shared

sealed trait Tile {
  def char: String
  def value: Int
  def id: Int
}

case class RegularTile(id: Int, char: String, value: Int) extends Tile

case class Blank(id: Int, maybeChar: Option[String] = None) extends Tile {
  def char  = maybeChar.getOrElse("")
  def value = 0
}
