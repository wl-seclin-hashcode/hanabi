package hanabi

import state.GameState
import SimpleRules._

trait StackedDeck {

  def stack(cards: Seq[Card], numPlayer: Int = 3, handSize: Int = 5): GameState = {
    val (hands, deck) = Deck(cards).deal(numPlayer, handSize)
    GameState(currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = allColors.map((_, 0)).toMap,
      discarded = Seq.empty,
      remainingHint = MAX_HINT,
      remainingLife = MAX_LIFE)
  }

  val orderedState = stack(allCards)
  val reverseState = stack(allCards.reverse)

  def trivialState(players: Int = 3, handSize: Int = 5) = stack(allCards.distinct ++ allCards, players, handSize)

}