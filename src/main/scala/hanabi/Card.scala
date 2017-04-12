package hanabi

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
case class Card(level: Int, color: Color) {
  def debugString = {
    val c = color.toString.head
    s"$level$c"
  }
}

object Cards {

  implicit class IntToCard(i: Int) {
    def R = Card(i, Red)
    def Y = Card(i, Yellow)
    def B = Card(i, Blue)
    def W = Card(i, White)
    def G = Card(i, Green)
  }
}