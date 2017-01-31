package bubbleships.model

import bubbleships.model.Ship._

object Ship {

  sealed trait Status

  case object Intact extends Status

  case object Hit extends Status

  sealed abstract class Type(val name: String, val size: Int)

  case object Carrier extends Type("Carrier", 5)

  case object BattleShip extends Type("BattleShip", 4)

  case object Destroyer extends Type("Destroyer", 3)

  case object Submarine extends Type("Submarine", 3)

  case object PatrolBoat extends Type("Patrol Boat", 2)

  val types = List(Carrier, BattleShip, Destroyer, Submarine, PatrolBoat)

  def apply(shipType: Type, pos: Pos, dir: Direction): Ship = Ship(shipType,
    List.iterate(pos, shipType.size)(_.inDirection(dir)).map(_ -> Intact).toMap)
}

case class Ship private(shipType: Type, positions: Map[Pos, Status]) {
  def sunk = positions.values.forall(_ == Ship.Hit)

  def shot(pos: Pos) = positions.get(pos)
    .filter(_ == Intact)
    .map(s => new Ship(shipType, positions + (pos -> Ship.Hit)))
}
