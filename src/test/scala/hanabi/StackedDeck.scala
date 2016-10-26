package hanabi

import hanabi.state.GameState
import SimpleRules._

trait StackedDeck {

  def stack(cards: Seq[Card], numPlayer: Int = 3, handSize: Int = 5): GameState = {
    val (hands, deck) = Deck(cards).deal(numPlayer, handSize)
    GameState(
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = allColors.map((_, 0)).toMap)
  }

  val orderedState = stack(allCards)
  val reverseState = stack(allCards.reverse)

  def trivialState(players: Int = 3, handSize: Int = 5) = stack(allCards.distinct ++ allCards, players, handSize)

}