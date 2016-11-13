package hanabi

case class Hand(cards: Vector[Card], clues: Vector[Clue] = Vector.empty) {
  def play(pos: Int) = {
    val (x, y) = cards.splitAt(pos)
    (y.head, Hand(x ++ y.tail))
  }
  def insert(c: Card, pos: Int) = {
    val (x, y) = cards.splitAt(pos)
    Hand(x ++ (c +: y))
  }
  def +(c: Card) = Hand(c +: cards)
  def +(c: Option[Card]) = Hand(c.toVector ++ cards)

  def hint(c: Color) = {
    val newClues = for {
      (card, pos) <- cards.zipWithIndex.toSeq
      if card.color == c
    } yield ColorClue(c, pos)
    copy(clues = clues ++ newClues)
  }

  def hint(l: Int) = {
    val newClues = for {
      (card, pos) <- cards.zipWithIndex.toSeq
      if card.level == l
    } yield LevelClue(l, pos)
    copy(clues = clues ++ newClues)
  }

}

object Hand {
  def apply(cards: Card*): Hand = Hand(cards.toVector)
}