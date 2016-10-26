package hanabi.state

import hanabi._

case class GameState(currentPlayer: Int,
                     deck: Deck,
                     private[state] val playersHands: IndexedSeq[Hand],
                     table: Map[Color, Int],
                     discarded: Seq[Card],
                     remainingHint: Int,
                     remainingLife: Int,
                     clues: Map[Int, Seq[Clue]] = Map.empty.withDefaultValue(Vector()),
                     rules: HanabiRules = SimpleRules,
                     turnsLeft: Option[Int] = None,
                     lastInfo: Option[Info] = None) {

  val numPlayer = playersHands.size
  private[state] def activeHand = playersHands(currentPlayer)

  val tableCards = for {
    (c, max) <- table
    l <- 1 to max
  } yield Card(l, c)

  def lost = remainingLife == 0
  def won = score == 25
  def finished = won || lost || turnsLeft == Some(0)

  def play(move: Move) = {
    move match {
      case h: Hint           => hint(h)
      case PlayCard(cardPos) => playCard(cardPos)
      case Discard(cardPos)  => discard(cardPos)
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

  private def nextPlayer: GameState = copy(currentPlayer = (currentPlayer + 1) % numPlayer)

  private def updateDraw(hand: Hand): GameState = {
    val (drawn, newDeck) = deck.draw
    updateHand(hand + drawn).copy(deck = newDeck).checkTurnsLeft
  }

  private def checkTurnsLeft =
    if (deck.isEmpty)
      copy(turnsLeft = turnsLeft orElse Some(playersHands.size + 1))
    else this

  private def updateHand(newHand: Hand): GameState = copy(playersHands = playersHands.updated(currentPlayer, newHand))

  private def hint(move: Hint) = {
    require(canHint)
    val id = move.playerId
    val newClues = hintToClues(move)
    copy(
      clues = clues + (id -> (clues(id) ++ newClues)),
      remainingHint = remainingHint - 1,
      lastInfo = Some(Clued(player = id, clues = newClues))).nextPlayer
  }

  private def hintToClues(h: Hint): Seq[Clue] = {
    def matchesClue(c: Card) = h match {
      case ColorHint(_, color) => c.color == color
      case LevelHint(_, level) => c.level == level
    }

    def buildHint(pos: Int) = h match {
      case ColorHint(_, color) => ColorClue(color, pos)
      case LevelHint(_, level) => LevelClue(level, pos)
    }

    for {
      (card, pos) <- playersHands(h.playerId).cards.zipWithIndex.toSeq
      if matchesClue(card)
    } yield buildHint(pos)
  }

  def cluesFor(player: Int): Seq[Clue] =
    clues(player)

  private def playCard(pos: Int) = {
    val (played, hand) = activeHand.play(pos)
    val success = played.level == table(played.color) + 1
    val r = if (success)
      copy(
        table = table.updated(played.color, played.level),
        remainingHint = if (played.level == 5 && remainingHint < MAX_HINT) remainingHint + 1 else remainingHint)
    else
      copy(
        remainingLife = remainingLife - 1,
        discarded = played +: discarded)

    val info = Played(currentPlayer, pos, played, success)
    r.updateDraw(hand).nextPlayer.copy(lastInfo = Some(info))
  }

  def canDiscard = remainingHint < MAX_HINT
  def canHint = remainingHint > 0

  private def discard(pos: Int) = {
    require(canDiscard)
    val (discardedCard, hand) = activeHand.play(pos)
    val info = Discarded(currentPlayer, pos, discardedCard)
    val r = copy(
      discarded = discardedCard +: discarded,
      remainingHint = if (remainingHint < MAX_HINT) remainingHint + 1 else remainingHint,
      lastInfo = Some(info))
    r.updateDraw(hand).nextPlayer
  }

  lazy val seenBy: Map[Int, Vector[Card]] = Map(
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
    | $remainingHint hints, $remainingLife lifes
    """.stripMargin
}

object GameState {
  def initial(numPlayer: Int, rules: HanabiRules = SimpleRules) = {
    val handSize = if (numPlayer <= 3) 5 else 4
    val (hands, deck) = Deck.shuffle(rules.allCards).deal(numPlayer, handSize)
    GameState(currentPlayer = 0,
      deck = deck,
      playersHands = hands.toIndexedSeq,
      table = rules.allColors.map((_, 0)).toMap,
      discarded = Seq.empty,
      remainingHint = MAX_HINT,
      remainingLife = MAX_LIFE)
  }
}