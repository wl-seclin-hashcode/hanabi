package hanabi.state

import hanabi._

case class GameState(
    deck: Deck,
    private[state] val playersHands: Seq[Hand],
    currentPlayer: Int = 0,
    table: Map[Color, Int] = Map.empty,
    discarded: Seq[Card] = Seq.empty,
    hints: Int = SimpleRules.MAX_HINTS,
    lives: Int = SimpleRules.INITIAL_LIVES,
    rules: HanabiRules = SimpleRules,
    turnsLeft: Option[Int] = None,
    lastInfo: Option[Info] = None) {

  import rules._

  val numPlayer = playersHands.size
  val handSize = playersHands.head.cards.size

  private[state] def activeHand = playersHands(currentPlayer)

  //other players hands are visible, first element is next player hand
  val inactiveHands = playersHands.drop(currentPlayer) ++ playersHands.take(currentPlayer-1)

  val tableCards = for {
    (c, max) <- table
    l <- 1 to max
  } yield Card(l, c)

  def lost = lives == 0
  def won = score == 25
  def finished = won || lost || turnsLeft.contains(0)

  def play(move: Move) = {
    move match {
      case h: Hint          => hint(h)
      case Play(cardPos)    => playCard(cardPos)
      case Discard(cardPos) => discard(cardPos)
    }
  }.decrTurnsLeft

  private def decrTurnsLeft = copy(turnsLeft = turnsLeft.map(_ - 1))

  def score = table.values.sum

  def isPlayable(c: Card) = {
    table.get(c.color) match {
      case Some(lvl) if lvl == c.level - 1 => true
      case _                               => false
    }
  }

  def nextPlayer = (currentPlayer + 1) % numPlayer

  private def toNextPlayer: GameState = copy(currentPlayer = nextPlayer)

  private def updateDraw(hand: Hand): GameState = {
    val (drawn, newDeck) = deck.draw
    updateHand(hand + drawn).copy(deck = newDeck).checkTurnsLeft
  }

  private def checkTurnsLeft =
    if (deck.empty)
      copy(turnsLeft = turnsLeft orElse Some(playersHands.size + 1))
    else this

  private def updateHand(newHand: Hand): GameState = copy(playersHands = playersHands.updated(currentPlayer, newHand))

  private def hint(move: Hint) = {
    require(canHint)
    val id = move.playerId
    val (cluedHand, newClues) = playersHands(move.playerId).hint(move)
    copy(playersHands = playersHands.updated(move.playerId, cluedHand),
      hints = hints - 1,
      lastInfo = Some(Clued(player = id, clues = newClues))).toNextPlayer
  }

  def cluesFor(player: Int): Seq[Clue] =
    playersHands(player).clues

  def allowed(c: Card) = c.level == table(c.color) + 1

  private def playCard(pos: Int) = {
    val (played, hand) = activeHand.play(pos)
    val success = allowed(played)
    val r = if (success)
      copy(
        table = table.updated(played.color, played.level),
        hints = if (played.level == 5 && hints < MAX_HINTS) hints + 1 else hints)
    else
      copy(
        lives = lives - 1,
        discarded = played +: discarded)

    val info = Played(currentPlayer, pos, played, success)
    r.updateDraw(hand).toNextPlayer.copy(lastInfo = Some(info))
  }

  def canDiscard = hints < MAX_HINTS
  def canHint = hints > 0

  private def discard(pos: Int) = {
    require(canDiscard)
    val (discardedCard, hand) = activeHand.play(pos)
    val info = Discarded(currentPlayer, pos, discardedCard)
    val r = copy(
      discarded = discardedCard +: discarded,
      hints = if (hints < MAX_HINTS) hints + 1 else hints,
      lastInfo = Some(info))
    r.updateDraw(hand).toNextPlayer
  }

  //must be hidden outside package to prevent cheats
  private[state] lazy val seenBy: Map[Int, Vector[Card]] = Map(
    (for {
      p <- 0 until playersHands.size
      (before, after) = playersHands.splitAt(p)
      others = before ++ after.drop(1)
      cards = others.flatMap(_.cards).toVector ++ tableCards ++ discarded
    } yield p -> cards): _*)

  def isUseless(c: Card) = table.getOrElse(c.color, 0) >= c.level
  def isKey(c: Card) = c.level == 5 || discarded.contains(c)

  def debugString = s"""
    | hands : ${playersHands.map(_.cards.map(_.debugString))}
    | discard : ${discarded.map(_.debugString)}
    | in play : $table
    | $hints hints, $lives lifes
    """.stripMargin
}

object GameState {
  def forced(hands: Seq[Hand], played: Map[Color, Int] = Map.empty, rules: HanabiRules = SimpleRules) = {
    GameState(deck = Deck(rules.allCards).drop(hands.flatMap(_.cards)), hands)
  }

  def initial(numPlayer: Int, rules: HanabiRules = SimpleRules) = {
    val handSize = if (numPlayer <= 3) 5 else 4
    val (hands, deck) = Deck.shuffle(rules.allCards).deal(numPlayer, handSize)
    GameState(currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = rules.allColors.map((_, 0)).toMap)
  }
}