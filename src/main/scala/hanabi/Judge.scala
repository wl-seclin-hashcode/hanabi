package hanabi

import hanabi.state._

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 20/09/16
 * Time: 15:36
 */
object Judge {
  def start(players: IndexedSeq[Player]) = Judge(players, GameState.initial(players.size))

  def avgScore(ai: Player, repeats: Int, playerCount: Int) = {
    val results = for {
      r <- 0 until repeats
      end = Judge.start(Vector.fill(playerCount)(ai)).playToTheEnd
    } yield end.score
    results.sum.toDouble / repeats
  }
}

case class Judge(players: IndexedSeq[Player], state: GameState) {
  def nextState = {
    val move = players(state.currentPlayer).nextMove(state)
    copy(state = state.play(move))
  }

  def playToTheEnd: GameState =
    if (state.finished) state else nextState.playToTheEnd

}