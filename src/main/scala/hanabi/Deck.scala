package hanabi

import scala.util.Random
import scala.annotation.tailrec

object Deck {
  def shuffle(allCards: Seq[Card]) = Deck(allCards).shuffle
}

case class Deck(cards: Seq[Card]) {
  def empty = cards.isEmpty
  def draw: (Option[Card], Deck) = (cards.headOption, Deck(cards.drop(1)))

  def drop(cs: Seq[Card]) = Deck(cards diff cs)

  def deal(hands: Int, cardsPerHand: Int): (Seq[Hand], Deck) = {

    @tailrec
    def dealRec(currentHands: Seq[Hand] = Seq.fill(hands)(Hand()), currentDeck: Deck = this, nbCards: Int = hands * cardsPerHand): (Seq[Hand], Deck) =
      if (nbCards == 0)
        (currentHands, currentDeck)
      else {
        val (card, deck) = currentDeck.draw
        val hand = currentHands.head + card
        dealRec(currentHands.tail :+ hand, deck, nbCards - 1)
      }

    dealRec()

  }

  def shuffle = Deck(Random.shuffle(cards))
}
