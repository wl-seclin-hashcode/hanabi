package hanabi

trait HanabiRules {
  def allowedColorHints: Set[Color]
  def allCards: Seq[Card]
  lazy val allColors: Set[Color] = allCards.map(_.color).toSet
  lazy val count: Map[Card, Int] = allCards.groupBy(identity).mapValues(_.size)

  lazy val withLevel: Map[Int, Seq[Card]] =
    (for (l <- 1 to 5)
      yield l -> allCards.filter(_.level == l).distinct).toMap
  lazy val withColor: Map[Color, Seq[Card]] =
    (for (c <- allColors)
      yield c -> allCards.filter(_.color == c).distinct).toMap

  val MAX_HINTS = 8
  val INITIAL_LIVES = 3
}

case object SimpleRules extends HanabiRules {
  val allowedColorHints: Set[Color] = Set(Yellow, Blue, Green, Red, White)
  val allCards: Seq[Card] = for {
    level <- Seq(1, 1, 1, 2, 2, 3, 3, 4, 4, 5)
    color <- Set(Yellow, Blue, Green, Red, White)
  } yield Card(level, color)
}
