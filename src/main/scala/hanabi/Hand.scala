package hanabi

case class Hand(cards: Vector[Card]) {
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
}

object Hand {
  def apply(cards: Card*): Hand = Hand(cards.toVector)
}