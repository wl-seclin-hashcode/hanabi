package hanabi

import hanabi.state._
import hanabi.state.GameState

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
    val next = state.play(move)
    for {
      p <- players
      i <- next.lastInfo
    } p.info(i)
    copy(state = next)
  }

  def playToTheEnd: GameState =
    if (state.finished) state else nextState.playToTheEnd

}

trait Info
case class Played(player: Int, pos: Int, card: Card, success: Boolean) extends Info
case class Discarded(player: Int, pos: Int, card: Card) extends Info
case class Clued(player: Int, clues:Seq[Clue]) extends Info