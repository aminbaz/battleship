package example

import scala.annotation.tailrec

case class Ship(typeShip: String, headSquare: List[Int], direction: String, lifePoint : Int) {
}

object Ship{
  /**
    * Get the size of a ship.
    *
    * Get the size by the type of  ship
    * @param typeShip : Type of the ship
    * @return The size of the ship
    */
  def getSize(typeShip : String): Int = {
    typeShip match {
      case "Carrier" => 5
      case "Battleship" => 4
      case "Cruiser" => 3
      case "Submarine" => 3
      case "Destroyer" => 2
      case _ => 0
    }
  }

  /**
    * Check the placement of one ship.
    *
    * Check the input from the players to place their ships
    * @param typeShip : Type of the ship
    * @param rowShip : Row coordinate of the ship
    * @param colShip : Col coordinate of the ship
    * @param dirShip : Direction of ship
    * @param ships : List of ship to check overlapping
    * @return Option of a ship, None if the placement is incorrect or Some(Ship) in other case
    */
  @tailrec
  def checkPlacement(typeShip : String,rowShip : Int, colShip : Int, dirShip : String,ships : List[Ship]) : Option[Ship] = {
    if(rowShip<0||rowShip>9||colShip<0||colShip>9){
      println("<!> The entire ship is out of bound.")
      None
    }else if((dirShip =="h"&&colShip+getSize(typeShip)>9)||(dirShip =="v"&&rowShip+getSize(typeShip)>9)){
      println("<!> The entire ship is out of bound.")
      None
    }else{
      if(ships.isEmpty){
        Some(Ship(typeShip,List(rowShip,colShip), dirShip,Ship.getSize(typeShip)))
      }else{
        val compareShip = ships.head
        dirShip match{
          case "h" => {
            if(compareShip.direction == "h"){
              if(rowShip==compareShip.headSquare(0)){
                if(List.range(colShip,colShip+getSize(typeShip)).intersect(List.range(compareShip.headSquare(1),compareShip.headSquare(1)+getSize(compareShip.typeShip))).isEmpty){
                  checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
                }else{
                  println("<!> Ships overlap, enter another value")
                  None
                }
              }else{
                checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
              }
            }else{
              val listRowTmp = List.range(compareShip.headSquare.head,compareShip.headSquare.head + getSize(compareShip.typeShip))
              if(listRowTmp.contains(rowShip)){
                val listColTmp = List.range(colShip,colShip + getSize(typeShip))
                if(listColTmp.contains(compareShip.headSquare(1))){
                  println("<!> Ships overlap, enter another value.")
                  None
                }else{
                  checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
                }
              }else{
                checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
              }
            }
          }
          case "v" => {
            if(compareShip.direction == "v"){
              if(colShip==compareShip.headSquare(1)){
                if(List.range(rowShip,rowShip+getSize(typeShip)).intersect(List.range(compareShip.headSquare(0),compareShip.headSquare(0)+getSize(compareShip.typeShip))).isEmpty){
                  checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
                }else{
                  println("<!> Ships overlap, enter another value")
                  None
                }
              }else{
                checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
              }
            }else{
              val listRowTmp = List.range(rowShip,rowShip + getSize(typeShip))
              if(listRowTmp.contains(compareShip.headSquare.head)){
                val listColTmp = List.range(compareShip.headSquare(1),compareShip.headSquare(1) + getSize(compareShip.typeShip))
                if(listColTmp.contains(colShip)){
                  println("<!> Ships overlap, enter another value.")
                  None
                }else{
                  checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
                }
              }else{
                checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
              }
            }
          }
        }
      }
    }
  }
}