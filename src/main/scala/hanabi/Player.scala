package hanabi

import hanabi.state.GameState

trait Player {
  def nextMove(state: GameState): Move
  def info(action: Info): Unit = {}

  def possibleCards(
    cardsSeen: Seq[Card],
    handSize: Int,
    clues: Seq[Clue] = Seq.empty,
    rules: HanabiRules = SimpleRules): Map[Int, Seq[Card]] = {

    def possibleCardsImpl(position: Int): Seq[Card] =
      for {
        card <- rules.allCards.diff(cardsSeen)
        if clues.forall(_.matches(card))
      } yield card

    for (p <- 0 until handSize)
      yield p -> possibleCardsImpl(p)
  }.toMap

}