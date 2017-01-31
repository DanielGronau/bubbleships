package bubbleships.model

sealed trait Result

case object Miss extends Result

case class Hit(shipType: Ship.Type, sunk: Boolean) extends Result


case class Board(ships: List[Ship], shots: Map[Pos, Result]) {

  def hasLost = ships.forall(_.sunk)

  def madeShot(pos: Pos, result: Result) = copy(shots = shots + (pos -> result))

  def receiveShot(pos: Pos) = ships
    .flatMap(_.shot(pos))
    .headOption
    .map(ship => Hit(ship.shipType, ship.sunk) ->
                 copy(ships = ship :: ships.filter(_.shot(pos).isEmpty)))
    .getOrElse(Miss -> this)

  override def toString = ships
    .map(ship => ship.shipType.name + ship.positions.keys.mkString("[", ",", "]"))
    .mkString(",")
}



