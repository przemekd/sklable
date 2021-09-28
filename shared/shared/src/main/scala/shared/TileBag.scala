package shared

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TileBag(private val supportedLang: SupportedLanguage, private val bag: ListBuffer[Tile]) {
  private val chars = bag
    .flatMap(tile =>
      tile.char.trim.length match {
        case 0 => None
        case _ => Some(tile.char)
      }
    )
    .distinct
    .toList
    .sorted(Collator.getInstance(supportedLang.languageCode))

  def getChars = chars

  def getTiles(n: Int) = {
    val number = if (n > bag.length) bag.length else n
    val tiles  = scala.util.Random.shuffle(bag).take(number).toList
    bag.subtractAll(tiles)
    tiles
  }

  def numberOfTilesLeft: Int =
    bag.size

  def exchangeTiles(tiles: List[Tile]) = {
    val newTiles = getTiles(tiles.length)
    for (tile <- tiles) bag.append(tile)
    newTiles
  }
}

object TileBag {
  def builder(): TileBagBuilder = new TileBagBuilder

  val emptyBag = builder().build

  def getTileBagByLang(lang: SupportedLanguage): TileBag =
    lang match {
      case SupportedLanguage.EN => englishTiles
      case SupportedLanguage.ES => spanishTiles
      case SupportedLanguage.PL => polishTilesBag
    }

  private def polishTilesBag =
    builder()
      .forLanguage(SupportedLanguage.PL)
      .addTiles("A", 1, 9)
      .addTiles("Ą", 5, 1)
      .addTiles("B", 3, 2)
      .addTiles("C", 2, 3)
      .addTiles("Ć", 6, 1)
      .addTiles("D", 2, 3)
      .addTiles("E", 1, 7)
      .addTiles("Ę", 5, 1)
      .addTiles("F", 5, 1)
      .addTiles("G", 3, 2)
      .addTiles("H", 3, 2)
      .addTiles("I", 1, 8)
      .addTiles("J", 3, 2)
      .addTiles("K", 2, 3)
      .addTiles("L", 2, 3)
      .addTiles("Ł", 3, 2)
      .addTiles("M", 2, 3)
      .addTiles("N", 1, 5)
      .addTiles("Ń", 7, 1)
      .addTiles("O", 1, 6)
      .addTiles("Ó", 5, 1)
      .addTiles("P", 2, 3)
      .addTiles("R", 1, 4)
      .addTiles("S", 1, 4)
      .addTiles("Ś", 5, 1)
      .addTiles("T", 2, 3)
      .addTiles("U", 2, 2)
      .addTiles("W", 1, 4)
      .addTiles("Y", 2, 4)
      .addTiles("Z", 1, 5)
      .addTiles("Ź", 9, 1)
      .addTiles("Ż", 5, 1)
      .addTiles(" ", 0, 2)
      .build

  private def englishTiles =
    builder()
      .forLanguage(SupportedLanguage.EN)
      .addTiles("E", 1, 12)
      .addTiles("A", 1, 9)
      .addTiles("I", 1, 9)
      .addTiles("O", 1, 8)
      .addTiles("N", 1, 6)
      .addTiles("R", 1, 6)
      .addTiles("T", 1, 6)
      .addTiles("L", 1, 4)
      .addTiles("S", 1, 4)
      .addTiles("U", 1, 4)
      .addTiles("D", 2, 4)
      .addTiles("G", 2, 3)
      .addTiles("B", 3, 2)
      .addTiles("C", 3, 2)
      .addTiles("M", 3, 2)
      .addTiles("P", 3, 2)
      .addTiles("F", 4, 2)
      .addTiles("H", 4, 2)
      .addTiles("V", 4, 2)
      .addTiles("W", 4, 2)
      .addTiles("Y", 4, 2)
      .addTiles("K", 5, 1)
      .addTiles("J", 8, 1)
      .addTiles("X", 8, 1)
      .addTiles("Q", 10, 1)
      .addTiles("Z", 10, 1)
      .addTiles(" ", 0, 2)
      .build

  private def spanishTiles =
    builder()
      .forLanguage(SupportedLanguage.ES)
      .addTiles(" ", 0, 2)
      .addTilesOfValue(1)(
        List(
          ("A", 12),
          ("E", 12),
          ("O", 9),
          ("I", 6),
          ("S", 6),
          ("N", 5),
          ("R", 5),
          ("U", 5),
          ("L", 4),
          ("T", 4)
        )
      )
      .addTilesOfValue(2)(
        List(
          ("D", 5),
          ("G", 2)
        )
      )
      .addTilesOfValue(3)(
        List(
          ("C", 4),
          ("B", 2),
          ("M", 2),
          ("P", 2)
        )
      )
      .addTilesOfValue(4)(
        List(
          ("H", 2),
          ("F", 1),
          ("V", 2),
          ("Y", 1)
        )
      )
      .addTilesOfValue(5)(
        List(
          ("CH", 1),
          ("Q", 1)
        )
      )
      .addTilesOfValue(8)(
        List(
          ("J", 1),
          ("LL", 1),
          ("Ñ", 1),
          ("RR", 1),
          ("X", 1)
        )
      )
      .addTiles("Z", 10, 1)
      .build
}

class TileBagBuilder {
  private val buffer = mutable.ListBuffer.empty[Tile]
  private var idx    = 1
  private var supportedLanguage: shared.SupportedLanguage = SupportedLanguage.EN

  def addTiles(char: String, value: Int, numberOfTiles: Int) = {
    buffer.addAll((0 until numberOfTiles).map { _ =>
      val tile = if (value == 0) Blank(idx) else RegularTile(idx, char, value)
      idx = idx + 1
      tile
    })
    this
  }

  def addTilesOfValue(value: Int)(tiles: List[(String, Int)]) = {
    tiles.foreach {
      case (char, numberOfTiles) =>
        this.addTiles(char, value, numberOfTiles)
    }
    this
  }

  def forLanguage(supportedLanguage: shared.SupportedLanguage) = {
    this.supportedLanguage = supportedLanguage
    this
  }

  def build = new TileBag(supportedLanguage, buffer)
}
