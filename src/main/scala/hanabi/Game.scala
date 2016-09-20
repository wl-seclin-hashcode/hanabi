package hanabi

import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 05/09/14
 * Time: 21:57
 */

object Hanabi {
  val allColors = Set(Yellow, Blue, Green, Red, White)
  val allCards = for {
    level <- Seq(1, 1, 1, 2, 2, 3, 3, 4, 4, 5)
    color <- allColors
  } yield Card(level, color)
  val MAX_HINT = 8
  val MAX_LIFE = 3
}


trait Color
case object Yellow extends Color
case object Blue extends Color
case object Green extends Color
case object Red extends Color
case object White extends Color

case class Card(level: Int, color: Color)

object Deck {
  def shuffle(allCards: Seq[Card]) = Deck(allCards).shuffle
}

case class Deck(cards: Seq[Card]) {
  def isEmpty = cards.isEmpty
  def nonEmpty = cards.nonEmpty
  def draw = (cards.head, Deck(cards.tail))
  def deal(hands: Int, cardsPerHand: Int) =
    (cards.grouped(cardsPerHand).take(hands).map(cards => Hand(cards.toIndexedSeq)),
      Deck(cards.drop(cardsPerHand*hands)))
  def shuffle = Deck(Random.shuffle(cards))
}

case class Hand(cards: IndexedSeq[Card]) {
  def play(pos: Int) = {
    val (x, y) = cards.splitAt(pos)
    (y.head, Hand(x ++ y.tail))
  }
  def insert(c: Card, pos: Int) = {
    val (x, y) = cards.splitAt(pos)
    Hand(x ++ (c +: y))
  }
  def +(c:Card) = Hand(cards :+ c)
}

case class State( currentPlayer : Int,
                  deck          : Deck,
                  playersHands  : IndexedSeq[Hand],
                  table         : Map[Color, Int],
                  discard       : Seq[Card],
                  remainingHint : Int,
                  remainingLife : Int)
{
  def numPlayer = playersHands.size
  def activeHand = playersHands(currentPlayer)

  private def nextPlayer: State =
    copy(currentPlayer = (currentPlayer + 1) % numPlayer)
  private def updateHand(newHand: Hand): State =
    copy(playersHands = playersHands.updated(currentPlayer, newHand))

  def hint = {
    require(remainingHint > 0)
    copy(remainingHint = remainingHint -1).nextPlayer
  }

  def play(pos: Int) = {
    val (played, hand) = activeHand.play(pos)
    val (drawn, newDeck) = deck.draw
    val success = played.level == table(played.color) + 1
    val r = if (success)
      copy(
        table = table.updated(played.color, played.level),
        deck = newDeck,
        remainingHint = if (played.level == 5 && remainingHint < Hanabi.MAX_HINT) remainingHint + 1 else remainingHint
      )
    else
      copy(
        deck = newDeck,
        remainingLife = remainingLife - 1,
        discard = played +: discard
      )

    r.updateHand(hand + drawn).nextPlayer
  }

  def discard(pos: Int) = {
    val (played, hand) = activeHand.play(pos)
    val (drawn, newDeck) = deck.draw
    val r = copy(
      deck = newDeck,
      discard = played +: discard,
      remainingHint = if (remainingHint < Hanabi.MAX_HINT) remainingHint + 1 else remainingHint
    )
    r.updateHand(hand + drawn).nextPlayer
  }

}

object State {
  def initial(numPlayer: Int) = {
    val handSize = if (numPlayer <= 3) 5 else 4
    val (hands, deck) = Deck.shuffle(Hanabi.allCards).deal(numPlayer, handSize)
    State( currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = Hanabi.allColors.map((_,0)).toMap,
      discard = Seq.empty,
      remainingHint = Hanabi.MAX_HINT,
      remainingLife = Hanabi.MAX_LIFE
    )
  }
}

class Game(numPlayer:Int) {
  require(numPlayer >= 2)
  require(numPlayer <= 5)
}