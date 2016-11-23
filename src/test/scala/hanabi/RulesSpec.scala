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
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class RulesSpec extends FlatSpec with Matchers with MockitoSugar with OneInstancePerTest with BeforeAndAfter
    with PropertyChecks with HanabiDomain {
  import SimpleRules._
  import Cards._

  "simple rules" should "have proper number of cards" in {
    forAll { c: Card =>
      whenever(c.level == 1) {
        count(c) shouldBe 3
      }
    }
    forAll { c: Card =>
      whenever(c.level == 5) {
        count(c) shouldBe 1
      }
    }
    forAll { c: Card =>
      whenever(c.level > 1 && c.level < 5) {
        count(c) shouldBe 2
      }
    }
  }

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100,
    maxDiscardedFactor = 15)
}