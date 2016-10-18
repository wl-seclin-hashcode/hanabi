package hanabi

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 20/09/16
 * Time: 15:36
 */
object Judge {
  def start(players: IndexedSeq[Player]) = Judge(players, GameState.initial(players.size))
}

case class Judge(players: IndexedSeq[Player], state: GameState) {
  def nextState = {
    val move = players(state.currentPlayer).nextMove(state)
    copy(state = state.play(move))
  }

  def playToTheEnd:GameState =
    if (state.finished) state else nextState.playToTheEnd

}