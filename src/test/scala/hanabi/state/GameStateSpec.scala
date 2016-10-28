package hanabi.state

import org.scalatest.mock.MockitoSugar

import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import hanabi._
import SimpleRules._

@RunWith(classOf[JUnitRunner])
class GameStateSpec extends FlatSpec
    with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter
    with StackedDeck {

  import Cards._
  val game = GameState.initial(4)

  "game state" should "initialize properly" in {
    def check(plCount: Int) = {
      val g = GameState.initial(plCount)
      for (hand <- g.playersHands) hand.cards should have size (if (plCount <= 3) 5 else 4)
      g.hints should be(8)
      g.lives should be(3)
      g.lost should be(false)
      g.playersHands should have size plCount
    }

    for { plCount <- 2 to 5 } check(plCount)
  }

  it should "count clues" in {
    val clued = game.play(ColorHint(0, Blue))
    clued.hints should be(7)
    clued.play(LevelHint(0, 3)).hints should be(6)
  }

  it should "not allow to discard at 8 clues" in {
    an[Exception] should be thrownBy game.play(Discard(0))
  }

  it should "stop after three mistakes" in {
    reverseState.play(Play(0)).play(Play(0)).play(Play(0)).lost should be(true)
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
    reverseState.cluesFor(2) shouldBe empty
    val clued = reverseState.play(ColorHint(2, Blue))
    clued.cluesFor(2) shouldBe (Vector(ColorClue(Blue, 2)))
    clued.cluesFor(0) shouldBe empty

    val again = clued.play(ColorHint(2, Red))
    again.cluesFor(2) shouldBe (Vector(ColorClue(Blue, 2), ColorClue(Red, 0)))
  }

  it should "list who sees which cards when no cards have been played or discarded" in {
    for {
      p <- 0 to 2
    } trivialState(3, 5).seenBy(p) should have size 10
  }

  it should "list who sees which cards when cards have been played" in {
    val plays = trivialState(3, 5).play(Play(0)).play(Play(0))
    for {
      p <- 0 to 2
    } plays.seenBy(p) should have size 12
  }

  it should "list who sees which cards when cards have been discarded" in {
    val plays = trivialState(3, 5).copy(hints = 3).play(Discard(0)).play(Discard(3)).play(Discard(2))
    for {
      p <- 0 to 2
    } plays.seenBy(p) should have size 13
  }

  it should "list who sees which cards when cards have been misplayed" in {
    val plays = trivialState(3, 5).copy(hints = 3).play(Play(0)).play(Play(3)).play(Discard(2))
    for {
      p <- 0 to 2
    } plays.seenBy(p) should have size 13
  }

  it should "have a force initialisation" in {
    val hands = Seq(
      Hand(1 R, 2 R, 4 B, 4 W, 5 R),
      Hand(1 G, 2 G, 4 Y, 4 Y, 5 Y))
    val state = GameState.forced(hands)

    state.deck.cards should contain noneOf (4 Y, 5 Y, 5 R)
  }
}