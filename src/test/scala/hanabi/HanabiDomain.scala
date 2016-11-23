package hanabi

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

trait HanabiDomain {
  val anyCard: Gen[Card] =
    for {
      l <- Gen.choose(1, 5)
      c <- Gen.oneOf(Blue, Red, Yellow, White, Green)
    } yield new Card(l, c)

  implicit val arbitraryCard = Arbitrary(anyCard)
}