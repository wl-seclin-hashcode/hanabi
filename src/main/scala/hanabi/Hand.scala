package hanabi

case class Hand(cards: Vector[Card], clues: Vector[Clue] = Vector.empty) {
  def play(pos: Int): (Card, Hand) = {
    val (before, after) = cards.splitAt(pos)
    val newClues = for {
      clue <- clues
      if clue.position != pos
      newPos = if (clue.position < pos) clue.position + 1 else clue.position
    } yield clue.update(newPos)

    (after.head, Hand(before ++ after.tail, newClues))
  }

  def +(c: Card) = copy(cards = c +: cards)
  def +(c: Option[Card]) = copy(cards = c.toVector ++ cards)

  def hint(m: Hint): (Hand, Seq[Clue]) = m match {
    case ColorHint(_, c) => hint(c)
    case LevelHint(_, l) => hint(l)
  }

  def hint(c: Color) = {
    val newClues = for {
      (card, pos) <- cards.zipWithIndex.toSeq
      if card.color == c
    } yield ColorClue(c, pos)
    (copy(clues = clues ++ newClues), newClues)
  }

  def hint(l: Int) = {
    val newClues = for {
      (card, pos) <- cards.zipWithIndex.toSeq
      if card.level == l
    } yield LevelClue(l, pos)
    (copy(clues = clues ++ newClues), newClues)
  }

}

object Hand {
  def apply(cards: Card*): Hand = Hand(cards.toVector)
}