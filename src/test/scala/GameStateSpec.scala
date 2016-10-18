
import org.scalatest.mock.MockitoSugar

import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.OneInstancePerTest
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import math._
import hanabi._

@RunWith(classOf[JUnitRunner])
class GameStateSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

  "game state" should "initialize properly" in {
    for (hand <- GameState.initial(3).playersHands) hand.cards should have size 5
    for (hand <- GameState.initial(4).playersHands) hand.cards should have size 4
  }

}