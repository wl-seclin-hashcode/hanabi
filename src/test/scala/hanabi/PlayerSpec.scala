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

  val onesToThrees = allCards.filter(_.level < 4)
  val foursFives = allCards.filter(_.level >= 4)
  val (fives, allExceptFives) = allCards.partition(_.level == 5)

  "a player" should "guess its possible cards with no clues" in {
    val possible = player.possibleCards(allExceptFives, 5)
    for (p <- 0 to 4)
      possible(p) should contain theSameElementsAs fives
  }

  it should "guess its possible cards with a single clue" in {
    val possible = player.possibleCards(onesToThrees, 5, Seq(LevelClue(level = 5, position = 3)))
    possible(1) should contain theSameElementsAs allCards.filter(_.level == 5)
  }

  val clues = Seq(
    LevelClue(position = 3, level = 5),
    ColorClue(position = 3, color = Blue))

  it should "guess its possible cards with a 2 clues on the same card" in {
    val possible = player.possibleCards(onesToThrees, 5, clues)
    possible(3) should contain theSameElementsAs Seq(Card(5, Blue))
  }

  //  it should "guess its possible cards with a 2 clues on another card" in {
  //    player.possibleCards(4, onesToThrees, clues) should
  //      contain theSameElementsAs (foursFives diff Seq(Card(5, Blue)))
  //  }
}