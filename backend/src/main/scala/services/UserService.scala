package services

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

final case class User(id: String, name: String, secret: String)

final case class Token(userId: String, token: String)

object UserService {
  val users  = mutable.IndexedBuffer.empty[User]
  val tokens = mutable.IndexedBuffer.empty[Token]

  def getUserById(id: String): Future[User] =
    Future {
      users.find(user => user.id == id) match {
        case Some(user) => user
        case None       => throw new Exception("Not found")
      }
    }

  def insertOrUpdateUser(id: String, name: String, secret: String): Future[User] =
    Future {
      val updatedUser = User(id, name, secret)
      val idx         = users.indexWhere(user => user.id == id)
      if (idx >= 0) {
        val user = users(idx)

        if (user.secret != secret) throw new Exception("Not authorized")

        users.update(idx, updatedUser)
        updatedUser
      } else {
        users.append(updatedUser)
        updatedUser
      }
    }

  def getToken(userId: String, userSecret: String): Future[String] =
    Future {
      val token = Random.alphanumeric.take(15).toList.mkString("")

      users.find(user => user.id == userId && user.secret == userSecret) match {
        case Some(_) =>
          tokens.append(Token(userId, token))
          token
        case None => throw new Exception("Not authorized")
      }
    }

  def getUserWithToken(token: String): Future[User] =
    Future {
      tokens.find(t => t.token == token) match {
        case Some(t) => users.find(u => u.id == t.userId).getOrElse(throw new Exception("Not found"))
        case None    => throw new Exception("Not found")
      }
    }

}
