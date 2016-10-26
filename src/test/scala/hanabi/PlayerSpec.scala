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
    for (p <- 0 to 4)
      player.possibleCards(p, allExceptFives) should contain theSameElementsAs fives
  }

  "a player" should "guess its possible cards with a single clue" in {
    val onesToThrees = allCards.filter(_.level < 4)
    player.possibleCards(3, onesToThrees, Seq(LevelClue(level = 5, position = 3))) should
      contain theSameElementsAs allCards.filter(_.level == 5)
  }

  "a player" should "guess its possible cards with a 2 clues on the same card" in {
    val onesToThrees = allCards.filter(_.level < 4)
    val clues = Seq(LevelClue(level = 5, position = 3), ColorClue(color = Blue, position = 3))
    player.possibleCards(3, onesToThrees, clues) should
      contain theSameElementsAs Seq(Card(5, Blue))
  }
}