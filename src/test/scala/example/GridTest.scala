package example
import org.scalatest._

class GridTest extends FunSuite with DiagrammedAssertions{
  val grid = Grid(List())
  val gridFilled = grid.gridFilled.updated(0,grid.gridFilled(0).updated(0,3))

  test(testName = "Check the update of a grid"){
    assert(Grid(List()).updateGrid(3,List(0,0))==gridFilled)
  }

  val ship = Ship(typeShip = "Carrier",List(0,0),"v",5)
  val gridShip = Grid(List(ship))
  var gridFilledByOneShip = gridShip.gridFilled
  gridFilledByOneShip = gridFilledByOneShip.updated(0,gridShip.gridFilled(0).updated(0,3))
  gridFilledByOneShip = gridFilledByOneShip.updated(1,gridShip.gridFilled(0).updated(0,3))
  gridFilledByOneShip = gridFilledByOneShip.updated(2,gridShip.gridFilled(0).updated(0,3))
  gridFilledByOneShip = gridFilledByOneShip.updated(3,gridShip.gridFilled(0).updated(0,3))
  gridFilledByOneShip = gridFilledByOneShip.updated(4,gridShip.gridFilled(0).updated(0,3))

  val gridTest = Grid(List(ship),gridFilled)

  test(testName = "Check if one ship as his position on th grid"){
    assert(Grid.placeShip(List(ship),List.fill(10)(List.fill(10)(0)))==gridFilledByOneShip)
  }


}
