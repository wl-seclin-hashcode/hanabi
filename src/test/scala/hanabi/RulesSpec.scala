package hanabi

import state._
import org.scalatest.mockito.MockitoSugar
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
  import Cards._

  "simple rules" should "have proper number of cards" in {
    count(1 R) shouldBe 3
    count(5 R) shouldBe 1
    count(2 B) shouldBe 2
  }
}