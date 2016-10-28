package hanabi.ai

import hanabi._
import hanabi.state.GameState

object StandardPlayer extends Player {

  def nextMove(state: GameState): Move = {
    import state.rules._

    val myClues = state.cluesFor(state.currentPlayer)
    myClues.sortBy(_.position).headOption match {
      case Some(ColorClue(color, pos)) => Play(pos)
      case Some(LevelClue(level, pos)) if withLevel(level).exists(state.allowed) => Play(pos)
      case _ => Discard(0)
    }
  }
}