
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
  import SimpleRules._
  val game = GameState.initial(4)

  val stacked = {
    val numPlayer = 3
    val handSize = 5
    val (hands, deck) = Deck(allCards.reverse).deal(numPlayer, handSize)
    GameState(currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = allColors.map((_, 0)).toMap,
      discarded = Seq.empty,
      remainingHint = MAX_HINT,
      remainingLife = MAX_LIFE)
  }

  "game state" should "initialize properly" in {
    def check(plCount: Int) = {
      val g=GameState.initial(plCount)
      for (hand <- g.playersHands) hand.cards should have size (if (plCount <= 3) 5 else 4)
      g.remainingHint should be(8)
      g.remainingLife should be(3)
      g.lost should be(false)
      g.playersHands should have size plCount
    }

    for { plCount <- 2 to 5 } check(plCount)
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