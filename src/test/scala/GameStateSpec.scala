
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

  "game state" should "initialize properly" in {
    for (hand <- GameState.initial(3).playersHands) hand.cards should have size 5
    for (hand <- game.playersHands) hand.cards should have size 4
    game.remainingHint should be(8)
    game.remainingLife should be(3)
  }

  it should "count clues" in {
    val clued = game.play(ColorHint(0, Blue))
    clued.remainingHint should be(7)
    clued.play(LevelHint(0, 3)).remainingHint should be(6)
  }

}