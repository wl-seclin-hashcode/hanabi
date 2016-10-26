package hanabi

import org.scalatest.mock.MockitoSugar

import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import hanabi._
import Card._
import hanabi.ai._
import state._
import org.mockito.Mockito._
import org.mockito.Matchers._
import SimpleRules._

@RunWith(classOf[JUnitRunner])
class JudgeSpec extends FlatSpec
    with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter
    with StackedDeck {

  "the Judge" should "handle dummy players who play in order" in {
    for (count <- 2 to 5) {
      val size = if (count <= 3) 5 else 4
      val orderedStack = stack(allCards, count, size)
      val reverseStacked = stack(allCards.reverse, count, size)
      val distinctStacked = trivialState(count, size)
      val players = Vector.fill(count)(DummyPlayer)
      Judge(players, orderedStack).state.numPlayer should be(count)
      Judge(players, orderedStack).playToTheEnd.score should not be 0
      Judge(players, reverseStacked).playToTheEnd.score shouldBe 0
      Judge(players, distinctStacked).playToTheEnd.score shouldBe 25
    }
  }

  it should "give a positive score to dummy player" in {
    for (playerCount <- 2 to 5) {
      val score = Judge.avgScore(DummyPlayer, 100, playerCount)
      println(s"avg score for $playerCount dummies : $score")
      score should be > 0.1
    }
  }

  it should "give a perfect score to cheater player" in {
    for (playerCount <- 2 to 5) {
      val score = Judge.avgScore(CheaterPlayer, 10, playerCount)
      println(s"avg score for $playerCount cheaters : $score")
      score should be > 23.0
    }
  }

  it should "notify players when a card is played" in {
    val p = mock[Player]
    val players = Vector(DummyPlayer, DummyPlayer, p)
    val judge = Judge(players, stack(allCards, 3, 5))
    judge.nextState.nextState
    verify(p).info(Played(player = 0, pos = 0, card = Card(1, White), success = true))
    verify(p).info(Played(player = 1, pos = 0, card = Card(1, Red), success = true))
  }

  it should "notify players when a card is discarded" in {
    val p0 = mock[Player]
    val p1 = mock[Player]
    val p2 = mock[Player]
    val p3 = mock[Player]
    val players = Vector(p0, p1, p2, p3)
    val judge = Judge(players, stack(allCards, 4, 4))
    when(p0.nextMove(any[GameState])).thenReturn(ColorHint(1, Blue))
    when(p1.nextMove(any[GameState])).thenReturn(ColorHint(2, Blue))
    when(p2.nextMove(any[GameState])).thenReturn(Discard(0))
    when(p3.nextMove(any[GameState])).thenReturn(Discard(0))

    judge.nextState.nextState.nextState.nextState

    for (p <- players) {
      verify(p).info(Discarded(player = 2, pos = 0, card = Card(1, Blue)))
      verify(p).info(Discarded(player = 3, pos = 0, card = Card(1, Green)))
    }
  }

  it should "notify players when a color clue is given" in {
    val p0 = mock[Player]
    val p1 = mock[Player]
    val players = Vector(p0, p1)
    val judge = Judge(players, stack(allCards, 2, 5))
    when(p0.nextMove(any[GameState])).thenReturn(ColorHint(1, Blue))

    judge.nextState
    for (p <- players) {
      verify(p).info(Clued(player = 1, clues = Seq(ColorClue(Blue, 3))))
    }
  }

  it should "notify players when an index clue is given" in {
    val p0 = mock[Player]
    val p1 = mock[Player]
    val players = Vector(p0, p1)
    val judge = Judge(players, stack(allCards, 2, 5))
    when(p0.nextMove(any[GameState])).thenReturn(LevelHint(playerId = 1, level = 1))

    judge.nextState
    for (p <- players) {
      val positions = 0 to 4
      val clues = positions.map(p => LevelClue(level = 1, position = p))
      verify(p).info(Clued(player = 1, clues))
    }
  }

}