package example

import org.scalatest._
class ShipTest extends FunSuite with DiagrammedAssertions {
  val nameShips = List("Carrier", "Battleship", "Cruiser", "Submarine","Destroyer")
  val ship = Ship(typeShip = nameShips.head,List(1,1),"v",Ship.getSize(nameShips.head))
  test(testName = "a ship should be created"){
    assert(ship.isInstanceOf[Ship])
  }
  test(testName = "the size 5 should be returned"){
    assert(Ship.getSize(ship.typeShip)==5)
  }
  test(testName = "the size 0 should be returned"){
    assert(Ship.getSize("Wrong Input")==0)
  }

  val ship1 = Ship(typeShip = nameShips.head,List(0,0),"v",Ship.getSize(nameShips.head))
  val ship2 = Ship(typeShip = nameShips.head,List(4,0),"v",Ship.getSize(nameShips.head))
  val ship3 = Ship(typeShip = nameShips.head,List(0,0),"h",Ship.getSize(nameShips.head))
  val ship4 = Ship(typeShip = nameShips.head,List(0,4),"h",Ship.getSize(nameShips.head))

  test(testName = "check the overlapping ship"){
    assert(Ship.checkPlacement(ship1.typeShip,ship1.headSquare(0), ship1.headSquare(1), ship1.direction,List(ship4)).get==ship1)
    assert(Ship.checkPlacement(ship4.typeShip,ship4.headSquare(0), ship4.headSquare(1), ship4.direction,List(ship1)).get==ship4)
    assert(Ship.checkPlacement(ship1.typeShip,ship1.headSquare(0), ship1.headSquare(1), ship1.direction,List(ship2)).isEmpty)
    assert(Ship.checkPlacement(ship3.typeShip,ship3.headSquare(0), ship3.headSquare(1), ship3.direction,List(ship4)).isEmpty)
    assert(Ship.checkPlacement(ship1.typeShip,ship1.headSquare(0), ship1.headSquare(1), ship1.direction,List(ship3)).isEmpty)
  }

  val ship5 = Ship(typeShip = nameShips.head,List(0,13),"h",Ship.getSize(nameShips.head))

  test(testName = "coordinate out of bound"){
    assert(Ship.checkPlacement(ship5.typeShip,-10, 0, ship5.direction,List(ship4)).isEmpty)
    assert(Ship.checkPlacement(ship5.typeShip,0, 15, ship5.direction,List(ship4)).isEmpty)
    assert(Ship.checkPlacement(ship5.typeShip,0, 8, ship5.direction,List(ship4)).isEmpty)
    assert(Ship.checkPlacement(ship2.typeShip,8, 0, ship2.direction,List(ship4)).isEmpty)
    assert(Ship.checkPlacement(ship5.typeShip,0, -10, ship5.direction,List(ship4)).isEmpty)
  }

}
