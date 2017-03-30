package hanabi.web

import hanabi.state.GameState
import hanabi.{Hand, LevelHint, Play}
import org.scalajs.dom.Element
import org.scalajs.jquery.jQuery

import scala.scalajs.js.JSApp

/**
  * Created with IntelliJ IDEA.
  * User: a203673
  * Date: 24/09/16
  * Time: 16:38
  */
object HanabiApp extends JSApp {

  def initUI = updateUI(GameState.initial(4)
    .play(Play(0))
    .play(LevelHint(0,1))
  )

  def updateUI(state: GameState) = {
    jQuery("#clues span").attr("class", (i: Int) ⇒ if (i < state.hints) "clue" else "used-clue")
    jQuery("#lives span").attr("class", (i: Int) ⇒ if (i < state.rules.INITIAL_LIVES - state.lives) "used-life" else "life")
    jQuery(".hand").each((i:Int, elem: Element) ⇒ updateHand(state.inactiveHands(i), elem))
    for { (color, level) ← state.table }
      jQuery(s"#table .card-$color").html(if (level==0) "&nbsp;" else level.toString)
    addUiClasses()
  }

  def updateHand(hand: Hand, elem: Element) = {
    val cards = jQuery(elem).children(".card")
    cards.attr("class", (i:Int) ⇒ s"card card-${hand.cards(i).color.toString}")
    cards.each((i:Int, card: Element) ⇒ jQuery(card).text(hand.cards(i).level.toString))
  }

  def addUiClasses() = {
    jQuery(".clue, .used-clue").addClass("glyphicon glyphicon-info-sign")
    jQuery(".life, .used-life").addClass("glyphicon glyphicon-remove-sign")
  }

  override def main() = {
    jQuery(initUI _)
  }
}
