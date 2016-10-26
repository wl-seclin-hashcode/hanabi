package hanabi.state

import org.scalatest.mock.MockitoSugar

import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import hanabi._

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
      val g = GameState.initial(plCount)
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

  it should "not allow to discard at 8 clues" in {
    an[Exception] should be thrownBy game.play(Discard(0))
  }

  it should "stop after three mistakes" in {
    stacked.play(PlayCard(0)).play(PlayCard(0)).play(PlayCard(0)).lost should be(true)
  }

  it should "stop one round after last card drawn" in {
    var g = game
    while (g.deck.cards.nonEmpty) {
      g = g.play(ColorHint(0, Blue))
      g = g.play(Discard(0))
      g.finished should be(false)
    }
    // last card drawn
    g.turnsLeft should be(Some(g.playersHands.size))
    for (i <- 1 until g.playersHands.size) {
      g = g.play(ColorHint(0, Blue))
      g.finished should be(false)
    }
    // last turn
    g = g.play(Discard(0))
    g.finished should be(true)
  }

  it should "keep given clues" in {
    stacked.cluesFor(2) shouldBe empty
    val clued = stacked.play(ColorHint(2, Blue))
    clued.cluesFor(2) shouldBe (Vector(ColorClue(Blue, 2)))
    clued.cluesFor(0) shouldBe empty

    val again = clued.play(ColorHint(2, Red))
    again.cluesFor(2) shouldBe (Vector(ColorClue(Blue, 2), ColorClue(Red, 0)))
  }

  it should "list who sees which cards when no cards have been played or discarded" in {
    for {
      p <- 0 to 2
    } stacked.seenBy(p) should have size 10
  }

//  it should "list who sees which cards when cards have been played and discarded" in {
//    for {
//      p <- 0 to 2
//    } stacked.seenBy(p) should have size 10
//  }

}