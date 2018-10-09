package example

case class Player(name : String, shipGrid : Grid, shotGrid : Grid, score : Int = 0, isAI : Boolean = false, coordShipTouchedAI : List[Int] = List(-1,-1), dirShootAI : List[String]= List("top","bottom","left","right")) {

}
object Player{
  /**
    * Handle inputs placement ships from the user.
    *
    * Check if inputs from the user are correct
    *@param codeInput : String to know which input is necessary (row,col,dir)
    */
  def initPlacement(codeInput : String) : String = {
    def checkInput(input : String) : Option[String] = {
      codeInput match {
        case "row"  =>
          try {
            if(input.toInt<1||input.toInt>10){None}else{Some(input)}
          } catch {
            case _: Exception =>
              println("<!> Your value is not an integer")
              None
          }
        case "col"  =>
          try {
            if(input.toInt<1||input.toInt>10){None}else{Some(input)}
          } catch {
            case _: Exception =>
              println("<!> Your value is not an integer")
              None

          }

        case "dir"  => if(input=="h"||input=="v"){Some(input)}else{None}
      }
    }
    codeInput match {
      case "row"  =>
        checkInput(readLine("Row coordinate of the head square of the ship => ")).getOrElse(
          {
            println("<!> Re-enter a value between 1 and 10")
            initPlacement("row")
          })
      case "col"  => checkInput(readLine("Col coordinate of the head square of the ship => ")).getOrElse(
        {
          println("<!> re-enter a value between 1 and 10")
          initPlacement("col")
        })
      case "dir"  => checkInput(readLine("Direction of the ship (h for horizontal or v for vertical) => ")).getOrElse(
        {
          println("<!> The choice is only between h and v")
          initPlacement("dir")
        })
    }
  }

  /**
    * Shoot on enemy's ships.
    *
    * Check if the shoot touched or not a enemy ship
    * @param rowShot : row coordinate of the shoot
    * @param colShot : col coordinate of the shoot
    * @param ships : list ships of the attacked player
    * @param shipGrid : Grid containing ships of the defensive player
    * @return Int (codeShoot -1 or -2 if already touched, 2 if touched) and Ship (ship touched)
    */
  def makeShot(rowShot : Int, colShot : Int, ships : List[Ship], shipGrid : Grid): (Int,Ship) ={
    var shipUpdated = ships.head
    ships.map(ship=>{
      val rowCoordinateShip = if(ship.direction=="h"){List(ship.headSquare(0))}else{List(ship.headSquare(0),ship.headSquare(0)+Ship.getSize(ship.typeShip))}
      val colCoordinateShip = if(ship.direction=="v"){List(ship.headSquare(1))}else{List.range(ship.headSquare(1),ship.headSquare(1)+Ship.getSize(ship.typeShip))}
      if(rowCoordinateShip.contains(rowShot)&&colCoordinateShip.contains(colShot)){
        shipUpdated = ship
        ship
      }
    })
    shipGrid.gridFilled(rowShot)(colShot) match {
      case 0 => (1,null)
      case 1 => (-1,null)
      case 2 => (-2,null)
      case 3 => (2,shipUpdated)
    }
  }
}