package hanabi

import scala.scalajs.js.annotation.JSExportAll

// clues are used to record information given by hints
trait Clue {
  val position: Int

  def update(newPos: Int): Clue

  def matches(c: Card): Boolean
}

@JSExportAll
case class ColorClue(color: Color, position: Int) extends Clue {
  def matches(c: Card) = c.color == color

  def update(newPos: Int) = copy(position = newPos)
}

@JSExportAll
case class LevelClue(level: Int, position: Int) extends Clue {
  def matches(c: Card) = c.level == level

  def update(newPos: Int) = copy(position = newPos)
}
