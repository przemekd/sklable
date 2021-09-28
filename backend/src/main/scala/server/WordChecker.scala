package server

import shared.SupportedLanguage
import shared.Tile
import scala.io.Source
import scala.io.Codec
import java.util.zip.GZIPInputStream
import scala.collection.immutable.HashSet

sealed abstract class WordChecker {
  protected val correctWords: HashSet[java.lang.String]
  protected def loadFromGzipFile(src: String): HashSet[java.lang.String] = {
    implicit val codec: Codec = Codec("UTF-8")
    val inputStream           = Thread.currentThread().getContextClassLoader.getResourceAsStream(src)
    val gzipFileSource        = Source.fromInputStream(new GZIPInputStream(inputStream))
    HashSet.from(gzipFileSource.getLines())
  }

  def wordExists(word: List[Tile]): Boolean =
    correctWords(word.map(_.char).mkString.toLowerCase())
}

class PolishWordChecker extends WordChecker {
  protected val correctWords: HashSet[java.lang.String] = loadFromGzipFile("dicts/pl.dict")
}

class EnglishWordChecker extends WordChecker {
  protected val correctWords: HashSet[java.lang.String] = loadFromGzipFile("dicts/en.dict")
}

class SpanishWordChecker extends WordChecker {
  protected val correctWords: HashSet[java.lang.String] = loadFromGzipFile("dicts/es.dict")

  override def wordExists(word: List[Tile]): Boolean =
    word.sliding(2).find { tiles =>
      if (
        (tiles.head.char.equalsIgnoreCase("L") && tiles.last.char.equalsIgnoreCase("L")) ||
        (tiles.head.char.equalsIgnoreCase("R") && tiles.last.char.equalsIgnoreCase("R")) ||
        (tiles.head.char.equalsIgnoreCase("C") && tiles.last.char.equalsIgnoreCase("H"))
      ) true
      else false
    } match {
      case Some(_) => false
      case None    => super.wordExists(word)
    }
}

object WordChecker {
  private val polishChecker  = new PolishWordChecker
  private val englishChecker = new EnglishWordChecker
  private val spanishChecker = new SpanishWordChecker

  def wordExists(dict: SupportedLanguage, word: List[Tile]): Boolean =
    dict match {
      case SupportedLanguage.EN => englishChecker.wordExists(word)
      case SupportedLanguage.ES => spanishChecker.wordExists(word)
      case SupportedLanguage.PL => polishChecker.wordExists(word)
    }

  def wordsExists(dict: SupportedLanguage, words: IterableOnce[List[Tile]]): Boolean =
    words.iterator.forall(w => wordExists(dict, w))
}
