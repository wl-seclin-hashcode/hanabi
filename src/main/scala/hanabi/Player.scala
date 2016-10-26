package hanabi

import hanabi.state.GameState

trait Player {
  def nextMove(state: GameState): Move
  def info(action: Info): Unit = {}
}