package hanabi

// clues are used to record information given by hints
trait Clue {
  val position: Int
}

case class ColorClue(color: Color, position: Int) extends Clue
case class LevelClue(level: Int, position: Int) extends Clue
