package hanabi.ai

import hanabi.state._
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
class StandardPlayerSpec extends FlatSpec
    with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter
    with StackedDeck {

  import SimpleRules._

  "std player" should "play a single color clued card" in {
    val board = orderedState.play(ColorHint(1, Blue))
    val move = StandardPlayer.nextMove(board)
    board.play(move).lastInfo shouldBe Some(Played(player = 1, pos = 2, card = Card(1, Blue), success = true))
  }

  it should "play another single color clued card" in {
    val board = orderedState.play(ColorHint(1, Red))
    val move = StandardPlayer.nextMove(board)
    board.play(move).lastInfo shouldBe Some(Played(player = 1, pos = 0, card = Card(1, Red), success = true))
  }

  it should "play the left-most '1' clued card" in {
    val board = orderedState.play(LevelHint(1, level=1))
    val move = StandardPlayer.nextMove(board)
    board.play(move).lastInfo shouldBe Some(Played(player = 1, pos = 0, card = Card(1, Red), success = true))
  }

  it should "not play '5' clued cards" in {
    val board = reverseState.play(LevelHint(1, level=5))
    val move = StandardPlayer.nextMove(board)
    
    move.isInstanceOf[Discard] shouldBe true 
  }
}