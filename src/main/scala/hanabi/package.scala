import scala.util.Random

/**
  * Created with IntelliJ IDEA.
  * User: a203673
  * Date: 20/09/16
  * Time: 15:13
  */
package object hanabi {
  val MAX_HINT = 8
  val MAX_LIFE = 3

  object Card {
    trait Color
    case object Yellow extends Color
    case object Blue extends Color
    case object Green extends Color
    case object Red extends Color
    case object White extends Color
    val allColors = Set(Yellow, Blue, Green, Red, White)
    val allCards = for {
      level <- Seq(1, 1, 1, 2, 2, 3, 3, 4, 4, 5)
      color <- allColors
    } yield Card(level, color)
  }

  case class Card(level: Int, color: Card.Color)

  object Deck {
    def shuffle(allCards: Seq[Card]) = Deck(allCards).shuffle
  }

  case class Deck(cards: Seq[Card]) {
    def isEmpty = cards.isEmpty
    def nonEmpty = cards.nonEmpty
    def draw = (cards.head, Deck(cards.tail))
    def deal(hands: Int, cardsPerHand: Int) =
      (for {
        player <- 0 until hands
      }
      yield {
        Hand(for {
          indexCard <- 0 until cardsPerHand
        }
        yield {
          cards(hands * indexCard + player)
        })
      },Deck(cards.drop(cardsPerHand*hands)))
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

  trait Player {
    def nextMove(state: GameState): Move
  }

  trait Move
  case class ColorHint(playerId: Int, color: Card.Color) extends Move
  case class LevelHint(playerId: Int, level: Int) extends Move
  case class PlayCard(cardPos: Int) extends Move
  case class Discard(cardPos: Int) extends Move
}
