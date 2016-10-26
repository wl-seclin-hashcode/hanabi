package hanabi

import hanabi.state.GameState

trait Player {
  def nextMove(state: GameState): Move
  def info(action: Info): Unit = {}

  def possibleCards(
    position: Int,
    cardsSeen: Seq[Card],
    clues: Seq[Clue] = Seq.empty,
    rules: HanabiRules = SimpleRules) =
    for {
      card <- rules.allCards.diff(cardsSeen)
      if clues.forall(_.matches(card))
    } yield card
}