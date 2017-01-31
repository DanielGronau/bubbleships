package bubbleships

import bubbleships.model.{Board, Player, SimpleAI}

object Main extends App {

  run(Game(new SimpleAI("Adam", 10, 10), new SimpleAI("Eve", 10, 10)))

  def run(game: Game): Unit = {
    println(s"${game.p1.name} has the ships ${game.b1}")
    println(s"${game.p2.name} has the ships ${game.b2}")
    Stream.iterate[Option[Game]](Some(game))(_.flatMap(_.shot()))
      .takeWhile(_.isDefined)
      .map(_.get)
      .force
  }
}

case class Game(p1: Player, p2: Player, b1: Board, b2: Board, move: Int) {
  def shot() = if (b1.hasLost) {
    println(s"${p2.name} has won")
    None
  } else {
    val pos = p1.nextShot(b1)
    val (result, newB2) = b2.receiveShot(pos)
    val newB1 = b1.madeShot(pos, result)
    println(s"${p1.name} shot to $pos with result $result")
    Some(Game(p2, p1, newB2, newB1, move + 1))
  }
}

object Game {
  def apply(p1: Player, p2: Player): Game =
    this (p1, p2, Board(p1.init, Map()), Board(p2.init, Map()), 0)
}

