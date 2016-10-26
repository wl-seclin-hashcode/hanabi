package hanabi

case class Card(level: Int, color: Color) {
  def debugString = {
    val c = color.toString.head
    s"$level$c"
  }
}