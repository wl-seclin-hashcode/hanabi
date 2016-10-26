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
import hanabi.ai._

@RunWith(classOf[JUnitRunner])
class PlayerSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {
  import SimpleRules._

  val player = new Player {
    def nextMove(s: GameState) = ???
  }

  "a player" should "guess its possible cards with no clues" in {
    val (fives, allExceptFives) = allCards.partition(_.level == 5)
    player.possibleCards(allExceptFives) should contain theSameElementsAs fives
  }
}