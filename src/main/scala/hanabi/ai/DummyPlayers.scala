package hanabi.ai

import hanabi._
import hanabi.state._
import hanabi.state.GameState

/**
 * Always plays his left-most card.
 */
object DummyPlayer extends Player {

  def nextMove(state: GameState): Move = {
    Play(0)
  }

}


