package bubbleships.model

case class Pos(x: Int, y: Int) {
  def inDirection(direction: Direction) = copy(
    x = x + direction.dx,
    y = y + direction.dy)

  override def toString: String = "" + " ABCDEFGHIJ".charAt(x) + y
}

sealed abstract class Direction(val dx: Int, val dy: Int)

case object Horizontal extends Direction(1, 0)

case object Vertical extends Direction(0, 1)

