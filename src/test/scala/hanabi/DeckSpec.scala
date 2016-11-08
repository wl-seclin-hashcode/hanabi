package hanabi

import state._

import org.scalatest.mock.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import hanabi._
import hanabi.Card._
import hanabi.ai._
import SimpleRules._

@RunWith(classOf[JUnitRunner])
class DeckSpec extends FlatSpec
    with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter
    with StackedDeck {

  "a Deck" should "distribute cards one by one" in {
    val (h, rest) = Deck(allCards.distinct).deal(hands = 5, cardsPerHand = 5)
    val hnds = h.toVector
    println(hnds)
    def expected(lvl: Int) = allColors.map(c => Card(lvl, c)).toVector
    for {
      h <- 0 until 5
      (c, i) <- hnds(h).cards.zipWithIndex
    } c.level should be(5 - i)
  }

  //  it should "allow to try to draw from an empty deck an return unchanged deck" in {
  //  }

}