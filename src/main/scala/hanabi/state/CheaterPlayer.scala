package hanabi.state

import hanabi._

object CheaterPlayer extends Player {

  def nextMove(state: GameState): Move = {
//    println(state.debugString)
    val myHand = state.activeHand.cards
    def discardOrHint =
      myHand.sortBy(- _.level).find(c => state.isUseless(c)|| !state.isKey(c)) match {
        case Some(c) if state.canDiscard => Discard(myHand.indexOf(c))
        case _ if state.canHint          => ColorHint(0, Blue)
        case _                           => Discard(myHand.size - 1)
      }
    myHand.find(c => state.isPlayable(c)) match {
      case Some(c) => PlayCard(myHand.indexOf(c))
      case _       => discardOrHint
    }
  }

}