package hanabi

import scala.util.Random

object Deck {
  def shuffle(allCards: Seq[Card]) = Deck(allCards).shuffle
}

case class Deck(cards: Seq[Card]) {
  def empty = cards.isEmpty
  def draw: (Option[Card], Deck) = (cards.headOption, Deck(cards.drop(1)))

  def drop(cs: Card*) = Deck(cards diff cs)

  def deal(hands: Int, cardsPerHand: Int): (Seq[Hand], Deck) =
    (Seq.tabulate(hands) { h =>
      Hand(Vector.tabulate(cardsPerHand) { i => cards(hands * i + h) })
    }, Deck(cards.drop(cardsPerHand * hands)))

  def shuffle = Deck(Random.shuffle(cards))
}
