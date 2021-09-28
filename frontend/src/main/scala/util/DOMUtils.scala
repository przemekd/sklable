package util

import org.scalajs.dom
import org.scalajs.dom.raw.Element
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable.ArrayBuffer

object DOMUtils {
  // https://gist.github.com/Rooster212/4549f9ab0acb2fc72fe3
  def elementsFromPoint(x: Double, y: Double): List[HTMLElement] = {
    val elements = new ArrayBuffer[Element]()
    val previousPointerEvents = new ArrayBuffer[(String, String)]()

    // get all elements via elementFromPoint, and remove them from hit-testing in order
    var element = dom.document.elementFromPoint(x, y)
    while (element != null && !elements.toList.contains(element)) {

      // push the element and its current style
      elements.addOne(element)

      previousPointerEvents += ((
        element.asInstanceOf[HTMLElement].style.getPropertyValue("pointer-events"),
        element.asInstanceOf[HTMLElement].style.getPropertyPriority("pointer-events")
      ))

      // add "pointer-events: none", to get to the underlying element
      element.asInstanceOf[HTMLElement].style.setProperty("pointer-events", "none", "important");
      element = dom.document.elementFromPoint(x, y)
    }

    // restore the previous pointer-events values
    previousPointerEvents.toList.reverse.zipWithIndex.foreach { case (t, i) =>
      elements(i).asInstanceOf[HTMLElement].style.setProperty("pointer-events", t._1, t._2)
    }

    // return our results
    elements.map(el => el.asInstanceOf[HTMLElement]).toList
  }

}

