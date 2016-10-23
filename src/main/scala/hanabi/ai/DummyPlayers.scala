package hanabi.ai

import hanabi._
import hanabi.state._

/**
 * Always plays his left-most card.
 */
object DummyPlayer extends Player {

  def nextMove(state: GameState): Move = {
    PlayCard(0)
  }

}


