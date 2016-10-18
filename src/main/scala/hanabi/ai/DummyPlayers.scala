package hanabi.ai

import hanabi._

object DummyPlayer extends Player {
  
  def nextMove(state: GameState): Move = {
    PlayCard(0)
  }
  
}