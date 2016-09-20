package hanabi

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 05/09/14
 * Time: 21:57
 */

case class GameState(currentPlayer : Int,
                     deck          : Deck,
                     playersHands  : IndexedSeq[Hand],
                     table         : Map[Card.Color, Int],
                     discard       : Seq[Card],
                     remainingHint : Int,
                     remainingLife : Int)
{
  def numPlayer = playersHands.size
  def activeHand = playersHands(currentPlayer)

  private def nextPlayer: GameState = copy(currentPlayer = (currentPlayer + 1) % numPlayer)

  private def updateHand(newHand: Hand): GameState = copy(playersHands = playersHands.updated(currentPlayer, newHand))

  def hint = {
    require(remainingHint > 0)
    copy(remainingHint = remainingHint -1).nextPlayer
  }

  def play(pos: Int) = {
    val (played, hand) = activeHand.play(pos)
    val (drawn, newDeck) = deck.draw
    val success = played.level == table(played.color) + 1
    val r = if (success)
      copy(
        table = table.updated(played.color, played.level),
        deck = newDeck,
        remainingHint = if (played.level == 5 && remainingHint < MAX_HINT) remainingHint + 1 else remainingHint
      )
    else
      copy(
        deck = newDeck,
        remainingLife = remainingLife - 1,
        discard = played +: discard
      )

    r.updateHand(hand + drawn).nextPlayer
  }

  def discard(pos: Int) = {
    val (played, hand) = activeHand.play(pos)
    val (drawn, newDeck) = deck.draw
    val r = copy(
      deck = newDeck,
      discard = played +: discard,
      remainingHint = if (remainingHint < MAX_HINT) remainingHint + 1 else remainingHint
    )
    r.updateHand(hand + drawn).nextPlayer
  }

}

object GameState {
  def initial(numPlayer: Int) = {
    val handSize = if (numPlayer <= 3) 5 else 4
    val (hands, deck) = Deck.shuffle(Card.allCards).deal(numPlayer, handSize)
    GameState( currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = Card.allColors.map((_,0)).toMap,
      discard = Seq.empty,
      remainingHint = MAX_HINT,
      remainingLife = MAX_LIFE
    )
  }
}