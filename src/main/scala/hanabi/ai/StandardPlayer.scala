package hanabi.ai

import hanabi._
import hanabi.state.GameState

object StandardPlayer extends Player {

  type Strategy = GameState => Option[Move]

  val play: Strategy = { state =>
    val myClues = state.cluesFor(state.currentPlayer)
    myClues.sortBy(_.position).headOption match {
      case Some(ColorClue(color, pos)) => Some(Play(pos))
      case Some(LevelClue(level, pos)) if state.rules.withLevel(level).exists(state.allowed) => Some(Play(pos))
      case _ => None
    }
  }

  val hint: Strategy = { state =>
    if (state.canHint)
      Some(LevelHint(state.nextPlayer, 1))
    else None
  }

  def nextMove(state: GameState): Move = {
    play(state).orElse(hint(state)).get
  }
}