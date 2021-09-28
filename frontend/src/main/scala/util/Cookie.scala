package util

import org.scalajs.dom
import scala.scalajs.js.Date

object Cookies {

  def read: Map[String, String] =
    dom.document.cookie
      .split(";")
      .toList
      .map(_.split("=").toList)
      .flatMap(x =>
        (x.headOption, x.drop(1).headOption) match {
          case (Some(k), Some(v)) => List((k.trim, v))
          case _ => Nil
        })
      .toMap

  def write(values: Map[String, String]): Unit = {
    val expiry = new Date(2120, 1)
    values.toList.foreach {
      case (k, v) =>
        val expires = expiry.toUTCString()
        dom.document.cookie = s"$k=$v;expires=$expires;path=/"
    }
  }
}
