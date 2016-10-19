package hanabi.state

import hanabi._

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 05/09/14
 * Time: 21:57
 */

case class GameState(currentPlayer: Int,
                     deck: Deck,
                     private[state] val playersHands: IndexedSeq[Hand],
                     table: Map[Color, Int],
                     discarded: Seq[Card],
                     remainingHint: Int,
                     remainingLife: Int,
                     rules: HanabiRules = SimpleRules,
                     turnsLeft: Option[Int] = None) {
  
  val numPlayer = playersHands.size
  private[state] def activeHand = playersHands(currentPlayer)

  def lost = remainingLife == 0
  def won = score == 25
  def finished = won || lost || turnsLeft == Some(0)

  def play(move: Move) = {
    move match {
      case _: LevelHint | _: ColorHint => hint
      case PlayCard(cardPos)           ⇒ playCard(cardPos)
      case Discard(cardPos)            ⇒ discard(cardPos)
    }
  }.decrTurnsLeft

  private def decrTurnsLeft = copy(turnsLeft = turnsLeft.map(_ - 1))

  def score = table.values.sum

  def isPlayable(c: Card) = {
    table.get(c.color) match {
      case Some(lvl) if lvl == c.level - 1 => true
      case _                               => false
    }
  }

  private def nextPlayer: GameState = copy(currentPlayer = (currentPlayer + 1) % numPlayer)

  private def updateDraw(hand: Hand): GameState = {
    val (drawn, newDeck) = deck.draw
    updateHand(hand + drawn).copy(deck = newDeck).checkTurnsLeft
  }

  private def checkTurnsLeft =
    if (deck.isEmpty)
      copy(turnsLeft = turnsLeft orElse Some(playersHands.size + 1))
    else this

  private def updateHand(newHand: Hand): GameState = copy(playersHands = playersHands.updated(currentPlayer, newHand))

  private def hint = {
    require(canHint)
    copy(remainingHint = remainingHint - 1).nextPlayer
  }

  private def playCard(pos: Int) = {
    val (played, hand) = activeHand.play(pos)
    val success = played.level == table(played.color) + 1
    val r = if (success)
      copy(
        table = table.updated(played.color, played.level),
        remainingHint = if (played.level == 5 && remainingHint < MAX_HINT) remainingHint + 1 else remainingHint)
    else
      copy(
        remainingLife = remainingLife - 1,
        discarded = played +: discarded)

    r.updateDraw(hand).nextPlayer
  }

  def canDiscard = remainingHint < MAX_HINT
  def canHint = remainingHint > 0

  private def discard(pos: Int) = {
    require(canDiscard)
    val (played, hand) = activeHand.play(pos)
    val r = copy(
      discarded = played +: discarded,
      remainingHint = if (remainingHint < MAX_HINT) remainingHint + 1 else remainingHint)
    r.updateDraw(hand).nextPlayer
  }

  def isUseless(c: Card) = table.getOrElse(c.color, 0) >= c.level
  def isKey(c: Card) = c.level == 5 || discarded.contains(c)

  def debugString = s"""
    | hands : ${playersHands.map(_.cards.map(_.debugString))}
    | discard : ${discarded.map(_.debugString)}
    | in play : $table
    | $remainingHint hints, $remainingLife lifes
    """.stripMargin
}

object GameState {
  def initial(numPlayer: Int, rules: HanabiRules = SimpleRules) = {
    val handSize = if (numPlayer <= 3) 5 else 4
    val (hands, deck) = Deck.shuffle(rules.allCards).deal(numPlayer, handSize)
    GameState(currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = rules.allColors.map((_, 0)).toMap,
      discarded = Seq.empty,
      remainingHint = MAX_HINT,
      remainingLife = MAX_LIFE)
  }
}