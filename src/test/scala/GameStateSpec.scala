
import org.scalatest.mock.MockitoSugar

import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import hanabi._
import Card._

@RunWith(classOf[JUnitRunner])
class GameStateSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {
  val game = GameState.initial(4)

  val stacked = {
    val numPlayer = 3
    val handSize = 5
    val (hands, deck) = Deck(Card.allCards.reverse).deal(numPlayer, handSize)
    GameState(currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = Card.allColors.map((_, 0)).toMap,
      discarded = Seq.empty,
      remainingHint = MAX_HINT,
      remainingLife = MAX_LIFE)
  }

  "game state" should "initialize properly" in {
    for (hand <- GameState.initial(3).playersHands) hand.cards should have size 5
    for (hand <- game.playersHands) hand.cards should have size 4
    game.remainingHint should be(8)
    game.remainingLife should be(3)
    game.lost should be(false)
  }

  it should "count clues" in {
    val clued = game.play(ColorHint(0, Blue))
    clued.remainingHint should be(7)
    clued.play(LevelHint(0, 3)).remainingHint should be(6)
  }

  it should "stop after three mistakes" in {
    stacked.play(PlayCard(0)).play(PlayCard(0)).play(PlayCard(0)).lost should be(true)
  }

}