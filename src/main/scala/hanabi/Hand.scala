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
  def +(c: Card) = Hand(cards :+ c)
  def +(c: Option[Card]) = Hand(cards ++ c)
}

object Hand {
  def apply(cards: Card*): Hand = Hand(cards.toVector)
}