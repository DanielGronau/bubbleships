package bubbleships.model

import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Player {
  val name: String

  def init: List[Ship]

  def nextShot(board: Board): Pos
}

class SimpleAI(val name: String, xSize: Int, ySize: Int) extends Player {

  private var possibleShots = Random.shuffle(
    for (x <- 1 to xSize; y <- 1 to ySize) yield Pos(x, y)).toList

  override def nextShot(board: Board) = {
    val pos = possibleShots.head
    possibleShots = possibleShots.tail
    pos
  }

  override def init: List[Ship] = {
    val buffer = ListBuffer[Ship]()
    Ship.types.foreach(t => {
      var found = false
      do {
        val pos = Pos(Random.nextInt(xSize) + 1, Random.nextInt(ySize) + 1)
        val dir = if (Random.nextBoolean()) Horizontal else Vertical
        val ship = Ship(t, pos, dir)
        found = ship.positions.keys.forall(p =>
          p.x <= xSize &&
          p.y <= ySize &&
          buffer.forall(_.shot(p).isEmpty))
        if (found) buffer += ship
      } while (!found)
    })
    buffer.toList
  }
}


