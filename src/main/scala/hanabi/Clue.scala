package hanabi

// clues are used to record information given by hints
trait Clue {
  val position: Int

  def matches(c: Card): Boolean
}

case class ColorClue(color: Color, position: Int) extends Clue {
  def matches(c: Card) = c.color == color
}
case class LevelClue(level: Int, position: Int) extends Clue {
  def matches(c: Card) = c.level == level
}
