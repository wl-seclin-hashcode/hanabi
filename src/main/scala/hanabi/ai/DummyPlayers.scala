package hanabi.ai

import hanabi._
import hanabi.state._

object DummyPlayer extends Player {

  def nextMove(state: GameState): Move = {
    PlayCard(0)
  }

}


