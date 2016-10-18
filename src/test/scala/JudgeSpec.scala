
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

@RunWith(classOf[JUnitRunner])
class JudgeSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

  def stack(cards: Seq[Card]): GameState = {
    val numPlayer = 3
    val handSize = 5
    val (hands, deck) = Deck(cards).deal(numPlayer, handSize)
    GameState(currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = Card.allColors.map((_, 0)).toMap,
      discarded = Seq.empty,
      remainingHint = MAX_HINT,
      remainingLife = MAX_LIFE)
  }

  val orderedStack = stack(Card.allCards)
  val reverseStacked = stack(Card.allCards.reverse)
  val distinctStacked = stack(Card.allCards.distinct ++ Card.allCards)

  "the Judge" should "handle dummy players who play in order" in {
    //TODO : does not work with 2 players
    for (count <- 2 to 5) {
      val players = Vector.fill(count)(DummyPlayer)
      Judge(players, orderedStack).playToTheEnd.score should not be 0
      Judge(players, reverseStacked).playToTheEnd.score shouldBe 0
      Judge(players, distinctStacked).playToTheEnd.score shouldBe 25
    }
  }

}