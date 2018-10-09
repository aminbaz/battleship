package example
import org.scalatest._

class PlayerTest extends FunSuite with DiagrammedAssertions{
  val ship = Ship(typeShip = "Carrier",List(0,0),"v",5)
  val grid = Grid(List(ship))
  var gridFilled = grid.gridFilled.updated(0,grid.gridFilled(0).updated(0,3))
  gridFilled = gridFilled.updated(0,grid.gridFilled(0).updated(0,3))
  gridFilled = gridFilled.updated(1,grid.gridFilled(0).updated(0,3))
  gridFilled = gridFilled.updated(2,grid.gridFilled(0).updated(0,3))
  gridFilled = gridFilled.updated(3,grid.gridFilled(0).updated(0,3))
  gridFilled = gridFilled.updated(4,grid.gridFilled(0).updated(0,3))
  gridFilled = gridFilled.updated(8,grid.gridFilled(0).updated(8,1))
  gridFilled = gridFilled.updated(9,grid.gridFilled(0).updated(9,2))

  val gridTest = Grid(List(ship),gridFilled)

  test(testName = "Check the result of player shot"){
    assert(Player.makeShot(0,0,List(ship),gridTest)==(2,ship))
    assert(Player.makeShot(1,0,List(ship),gridTest)==(2,ship))
    assert(Player.makeShot(2,0,List(ship),gridTest)==(2,ship))
    assert(Player.makeShot(9,0,List(ship),gridTest)==(1,null))
    assert(Player.makeShot(5,5,List(ship),gridTest)==(1,null))
    assert(Player.makeShot(8,8,List(ship),gridTest)==(-1,null))
    assert(Player.makeShot(9,9,List(ship),gridTest)==(-2,null))
  }
}