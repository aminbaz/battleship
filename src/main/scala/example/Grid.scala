package example

import scala.annotation.tailrec
import Console._


case class Grid(ships: List[Ship], gridFilled : List[List[Int]] = List.fill(10)(List.fill(10)(0))) {
  /**
    * Update the grid which will be displayed
    *
    * Fill the grid by codeColor (1 : miss, 2 : touched, 3 ships)
    * @param codeColor : 1 => miss, 2 => touched, 3 => ships
    * @param coordinate : List of 2 Int, the row and col coordinate
    * @return grid updated
    */
  def updateGrid(codeColor : Int, coordinate : List[Int]): List[List[Int]] ={
    this.gridFilled.updated(coordinate(0),this.gridFilled(coordinate(0)).updated(coordinate(1),codeColor))
  }

  /**
    * Ask the user to place his ships.
    *
    * Fill the ship list of the player
    * @param listNameShips : List of the different type of ship
    * @param ships : list of ships which will be filled
    * @return list of ships
    */
  def createShips(listNameShips : List[String], ships : List[Ship]) : List[Ship] = {
    if(listNameShips.isEmpty){
      ships
    }else{
      println("Create the ship : "+listNameShips.head)
      val coorRow = Player.initPlacement(codeInput = "row").toInt -1
      val coorCol = Player.initPlacement(codeInput = "col").toInt -1
      val direction = Player.initPlacement(codeInput = "dir")
      val ship = Ship.checkPlacement(listNameShips.head,coorRow,coorCol,direction,ships)

      if(ship.isEmpty){
        createShips(listNameShips, ships)
      }else{
        createShips(listNameShips.tail, ships :+ ship.get)
      }
    }
  }
}

object Grid {
  /**
    * Place ships on the grid.
    *
    * Fill the grid with ships entered by the player
    * @param ships : list of ships to place
    * @param gridFilled : Matrix which will contain information of the placement of ships
    * @return Matrix of codeColor
    */
  @tailrec
  def placeShip(ships: List[Ship], gridFilled: List[List[Int]]): List[List[Int]] = {
    def changeOneLine(line: List[Int], cols: List[Int]): List[Int] = {
      if (cols.isEmpty) {
        line
      } else {
        changeOneLine(line.updated(cols.head, 3), cols.tail)
      }
    }

    if (ships.isEmpty) {
      gridFilled
    }
    else {
      val ship = ships.head
      val rowCoordinates = if (ship.direction == "h") {
        List(ship.headSquare(0))
      } else {
        List.range(ship.headSquare(0), ship.headSquare(0) + Ship.getSize(ship.typeShip))
      }
      val colCoordinates = if (ship.direction == "v") {
        List(ship.headSquare(1))
      } else {
        List.range(ship.headSquare(1), ship.headSquare(1) + Ship.getSize(ship.typeShip))
      }
      var gridUpdated = gridFilled
      rowCoordinates.foreach(row => {
        gridUpdated = gridUpdated.updated(row, changeOneLine(gridUpdated(row), colCoordinates))
      })
      placeShip(ships.tail, gridUpdated)
    }
  }

  /**
    * Print a grid.
    *
    * Provide a visualization of the grid
    * @param grid : Matrix of Int
    */
  def printGrid(grid: List[List[Int]]): Unit = {
    grid.foreach(row => {
      row.foreach(col => {
        col match {
          case 0 => print(s"${RESET}${WHITE_B}   ${RESET}"+ "  | ")
          case 1 => print(s"${RESET}${RED_B}   ${RESET}"+ "  | ")
          case 2 => print(s"${RESET}${GREEN_B}   ${RESET}"+ "  | ")
          case 3 => print(s"${RESET}${BLUE_B}   ${RESET}"+ "  | ")
        }
      })
      println()
    })
  }
}