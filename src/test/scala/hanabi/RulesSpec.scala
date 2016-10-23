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
class RulesSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter {
  import SimpleRules._

  "simple rules" should "have proper number of cards" in {
    count(Card(1, Red)) shouldBe 3
    count(Card(5, Red)) shouldBe 1
    count(Card(2, Blue)) shouldBe 2
  }
}