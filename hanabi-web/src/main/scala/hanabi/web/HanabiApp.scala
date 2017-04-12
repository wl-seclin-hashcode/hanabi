package hanabi.web

import com.felstar.scalajs.vue.{Vue, VueOptions}
import hanabi._
import hanabi.state.GameState
import org.scalajs.jquery.jQuery

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.ScalaJSDefined
import scala.scalajs.js.{Dictionary, JSApp}

/**
  * Created with IntelliJ IDEA.
  * User: a203673
  * Date: 24/09/16
  * Time: 16:38
  */
object HanabiApp extends JSApp {

  val rules = SimpleRules
  var state = GameState.initial(2, rules)

  def toPlayerId(playerIdx: Int): Int = (state.currentPlayer + playerIdx + 1) % state.numPlayer

  def initUI() = {
    Vue.component("hanabi-card", literal(
      template = """<span class="card" :class="'card-' + card.color">{{card.level$1 == 0 ? '&nbsp;' : card.level}}</span>""",
      props = js.Array("card")
    ))
    val vm = new Vue(new VueOptions {
      override val el = "#main"

      override val data = new HanabiVue() {
        override val allowedColorHints = rules.allowedColorHints.toJSArray
      }
      override val methods = Dictionary[js.ThisFunction](
        "levelHint" → ((vm: HanabiVue, playerIdx: Int, level: Int) ⇒
          play(vm, LevelHint(toPlayerId(playerIdx), level))),
        "colorHint" → ((vm: HanabiVue, playerIdx: Int, color: Color) ⇒
          play(vm, ColorHint(toPlayerId(playerIdx), color))),
        "trash" → ((vm: HanabiVue, cardIdx: Int) ⇒
          play(vm, Discard(cardIdx))),
        "play" → ((vm: HanabiVue, cardIdx: Int) ⇒
          play(vm, Play(cardIdx)))
      )
    }).asInstanceOf[HanabiVue with Vue]

    updateUI(vm)
  }

  def updateUI(vm: HanabiVue) = {
    def cluesByPosition(clues: Seq[Clue]) = clues.groupBy(_.position.toString).mapValues(_.toJSArray).toJSDictionary

    jQuery("#clues span").attr("class", (i: Int) ⇒ if (i < state.hints) "clue" else "used-clue")
    jQuery("#lives span").attr("class", (i: Int) ⇒ if (i < state.rules.INITIAL_LIVES - state.lives) "used-life" else "life")
    jQuery(".clue, .used-clue").addClass("glyphicon glyphicon-info-sign")
    jQuery(".life, .used-life").addClass("glyphicon glyphicon-remove-sign")
    //jQuery(".hand").each((i: Int, elem: Element) ⇒ updateHand(state.inactiveHands(i), elem))
    for {(color, level) ← state.table}
      jQuery(s"#table .card-$color").html(if (level == 0) "&nbsp;" else level.toString)

    vm.oppHands = state.inactiveHands.map { hand ⇒
            literal(
              cards = hand.cards.toJSArray,
              clues = cluesByPosition(hand.clues)
            ): js.Object
          }.toJSArray
    vm.discard = state.discarded.toJSArray
    vm.myClues = cluesByPosition(state.cluesFor(state.currentPlayer))
  }

  def play(vm: HanabiVue, move: Move) = {
    println(s"Play Move $move")
    state = state.play(move)
    updateUI(vm)
  }

  override def main() = {
    jQuery(initUI _)
  }
}

@ScalaJSDefined
class HanabiVue extends js.Object {
  var oppHands: js.Array[js.Object] = js.Array()
  var myClues: js.Dictionary[js.Array[Clue]] = js.Dictionary()
  var discard: js.Array[Card] = js.Array()
  val allowedColorHints: js.Array[Color] = js.Array()
}