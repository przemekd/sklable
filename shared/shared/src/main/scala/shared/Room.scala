package shared

import scala.util.Random

object Room {
  def getRandomId() = {
    val part1 = Random.alphanumeric.take(4).toList.mkString("").toLowerCase()
    val part2 = Random.alphanumeric.take(4).toList.mkString("").toLowerCase()
    part1 + '-' + part2
  }

  val ID_REGEX = """[a-z0-9]{4}-[a-z0-9]{4}""".r

}
