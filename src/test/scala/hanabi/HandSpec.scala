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
import Cards._

@RunWith(classOf[JUnitRunner])
class HandSpec extends FlatSpec
    with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {

  "a Hand" should "convert color hints to clues" in {
    val h = Hand(2 R, 2 G, 3 B, 1 Y)
    val clued = h.hint(Blue)
    h.clues shouldBe empty
    clued.clues shouldBe (Vector(ColorClue(Blue, 2)))
  }

  it should "convert level hints to clues" in {
    val h = Hand(2 R, 2 G, 3 B, 1 Y)
    val clued = h.hint(2)
    h.clues shouldBe empty
    clued.clues shouldBe (Vector(LevelClue(2, 0), LevelClue(2, 1)))
  }

}