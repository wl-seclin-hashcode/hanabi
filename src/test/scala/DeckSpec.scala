
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
class DeckSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

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

  "a Deck" should "distribute cards one by one" in {
    val (h, rest) = Deck(Card.allCards.distinct).deal(hands = 5, cardsPerHand = 5)
    val hnds = h.toVector
    def expected(lvl: Int) = allColors.map(c => Card(lvl, c)).toVector
    for {
      h <- 0 until 5
      (c, i) <- hnds(h).cards.zipWithIndex
    } c.level should be(i + 1)

  }

}