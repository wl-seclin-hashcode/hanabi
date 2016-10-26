package hanabi

sealed trait Move

// a Hint can be given to a player to notify him of position(s) of color or values. 
// It can be given even if no cards match the hint.
sealed trait Hint extends Move { val playerId: Int }
case class ColorHint(playerId: Int, color: Color) extends Hint
case class LevelHint(playerId: Int, level: Int) extends Hint

case class Play(cardPos: Int) extends Move
case class Discard(cardPos: Int) extends Move
