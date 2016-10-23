import scala.util.Random
import hanabi.state._

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 20/09/16
 * Time: 15:13
 */
package object hanabi {
  val MAX_HINT = 8
  val MAX_LIFE = 3

  trait Color
  case object Yellow extends Color
  case object Blue extends Color
  case object Green extends Color
  case object Red extends Color
  case object White extends Color

  case class Card(level: Int, color: Color) {
    def debugString = {
      val c = color.toString.head
      s"$level$c"
    }
  }

  object Deck {
    def shuffle(allCards: Seq[Card]) = Deck(allCards).shuffle
  }

  case class Deck(cards: Seq[Card]) {
    def isEmpty = cards.isEmpty
    def nonEmpty = cards.nonEmpty
    def draw: (Option[Card], Deck) = (cards.headOption, Deck(cards.drop(1)))

    def deal(hands: Int, cardsPerHand: Int): (Seq[Hand], Deck) =
      (Seq.tabulate(hands) { h =>
        Hand(Vector.tabulate(cardsPerHand) { i => cards(hands * i + h) })
      }, Deck(cards.drop(cardsPerHand * hands)))

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
    def +(c: Card) = Hand(cards :+ c)
    def +(c: Option[Card]) = Hand(cards ++ c)
  }

  // clues are used to record information given by hints
  trait Clue {
    val position: Int
  }

  case class ColorClue(color: Color, position: Int) extends Clue
  case class LevelClue(level: Int, position: Int) extends Clue

  trait Player {
    def nextMove(state: GameState): Move
    def info(action: Info): Unit = {}
  }

  trait Move
  //a Hint can be given to a player to notify him of position(s) of color or values. It can be given even if no cards match the hint.
  trait Hint extends Move { val playerId: Int }
  case class ColorHint(playerId: Int, color: Color) extends Hint
  case class LevelHint(playerId: Int, level: Int) extends Hint
  case class PlayCard(cardPos: Int) extends Move
  case class Discard(cardPos: Int) extends Move

  trait HanabiRules {
    def allowedColorHints: Set[Color]
    def allCards: Seq[Card]
    lazy val allColors: Set[Color] = allCards.map(_.color).toSet
    lazy val count: Map[Card, Int] = allCards.groupBy(identity).mapValues(_.size)
  }

  case object SimpleRules extends HanabiRules {
    val allowedColorHints: Set[Color] = Set(Yellow, Blue, Green, Red, White)
    val allCards: Seq[Card] = for {
      level <- Seq(1, 1, 1, 2, 2, 3, 3, 4, 4, 5)
      color <- Set(Yellow, Blue, Green, Red, White)
    } yield Card(level, color)
  }
}
