package hanabi.ai

import hanabi._
import hanabi.state._
import hanabi.state.GameState

/**
 * Always plays his right-most card.
 */
object DummyPlayer extends Player {

  def nextMove(state: GameState): Move = {
    Play(state.handSize - 1)
  }

}


