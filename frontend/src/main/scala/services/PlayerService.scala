package services

import sttp.client.quick._
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.js
import java.{ util => ju }
import util.Cookies
import com.raquo.airstream.state.Var

case class User(id: String, name: String, secret: String)

case class Player(id: Player.PlayerId, name: String, secret: String)

object Player {
  type PlayerId = ju.UUID
}

object PlayerService {
  private val idKey     = "id"
  private val nameKey   = "name"
  private val secretKey = "secret"

  import scala.concurrent.ExecutionContext.Implicits.global

  private val apiPrefix = s"${dom.document.location.protocol}//${dom.document.location.host}/api"

  private val localPlayer = getLocalPlayer()

  private def generateNewPlayer =
    Player(ju.UUID.randomUUID(), name = "Sklable Player", secret = ju.UUID.randomUUID().toString)

  def getLocalPlayer(): Var[Player] = {
    val cookies = Cookies.read

    val player = (cookies.get(idKey), cookies.get(nameKey), cookies.get(secretKey))

    if (localPlayer != null)
      localPlayer
    else
      player match {
        case (Some(id), Some(name), Some(secret)) => Var(Player(ju.UUID.fromString(id), name, secret))
        case _                                    => Var(updateLocalPlayer(generateNewPlayer))
      }
  }

  private def updateLocalPlayer(player: Player): Player = {
    println("Updating cookie")
    Cookies.write(Map(idKey -> player.id.toString, nameKey -> player.name, secretKey -> player.secret))
    if (localPlayer != null) {
      localPlayer.set(player)
      println("Var[Player] set up.")
    }
    player
  }

  def validateUserName(name: String): List[String] = {
    val buf = new scala.collection.mutable.ListBuffer[String]()
    if ((name.length() < 5)) buf += "User name is too short."
    if ((name.length() > 25)) buf += "User name is too long."
    buf.toList
  }

  def isUserNameValid(name: String): Boolean =
    !(validateUserName(name).length > 0 || name.compareToIgnoreCase("Sklable Player") == 0)

  def insertOrUpdateUser(name: String): Future[String] = {
    if (!isUserNameValid(name)) return Future.failed(new Exception("Invalid name!"))

    val p = getLocalPlayer().now()

    val request = basicRequest
      .body(s"""{"id":"${p.id}", "name":"${name}", "secret":"${p.secret}"}""")
      .contentType("application/json")
      .post(uri"$apiPrefix/users")
      .response(asString)

    val response = request.send()

    response map {
      _.body match {
        case Left(error) => throw js.JavaScriptException(error)
        case Right(response) =>
          updateLocalPlayer(p.copy(name = name))
          response
      }
    }
  }

  def getToken(userId: String, userSecret: String): Future[String] = {
    val request = basicRequest
      .body(s"""{"userId":"$userId", "userSecret":"$userSecret"}""")
      .contentType("application/json")
      .post(uri"$apiPrefix/get_token")
      .response(asString)

    val response = request.send()

    response map {
      _.body match {
        case Left(error)  => throw js.JavaScriptException(error)
        case Right(token) => token
      }
    }
  }

}
