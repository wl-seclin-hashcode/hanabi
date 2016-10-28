package hanabi.ai

import hanabi._
import hanabi.state.GameState

object StandardPlayer extends Player {

  def nextMove(state: GameState): Move = {
    val myClues = state.cluesFor(state.currentPlayer)
    myClues.sortBy(_.position).headOption match {
      case Some(ColorClue(color, pos)) => Play(pos)
      case _                           => Discard(0)
    }
  }
}