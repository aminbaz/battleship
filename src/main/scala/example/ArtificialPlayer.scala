package example

import java.io.{BufferedWriter, FileWriter}
import scala.annotation.tailrec

case class ArtificialPlayer() {

}
object ArtificialPlayer{
  /**
    * Create ship for the AIPlayers.
    *
    * Create a random placement for the AI players
    * @param r : seed to generate random coordinates
    * @param listNameShips : List of specific type of ship
    * @param ships : List which will contain the ships created
    * @return List of created ships
    */
  @tailrec
  def createShips(r : scala.util.Random, listNameShips : List[String], ships : List[Ship]) : List[Ship] = {
    if(listNameShips.isEmpty){
      ships
    }else{
      val direction = if(r.nextInt() % 2 == 0){"h"}else{"v"}
      val ship = Ship.checkPlacement(listNameShips.head,r.nextInt(10),r.nextInt(10),direction,ships)
      ship match {
        case None => createShips(r, listNameShips, ships)
        case _ => {
          createShips(r, listNameShips.tail, ships :+ ship.get)
        }
      }
    }
  }

  /**
    * Handle shots of the AI players.
    *
    * Generate random shoots for the easy AI player
    * @param r : seed to generate random coordinates
    * @param ships : List of ships of the other player
    * @param shipGrid : Grid which contain information about the placement of each ship
    * @return The result of the shoot (codeColor), the ship which have been touch and the coordinate of the shoot
    */
  def shotAIEasy(r : scala.util.Random, ships : List[Ship], shipGrid : Grid): (Int,Ship,Int,Int) ={
    val shotRow = r.nextInt(10)
    val shotCol = r.nextInt(10)
    val result = Player.makeShot(shotRow,shotCol,ships,shipGrid)
    (result._1,result._2,shotRow,shotCol)
  }

  /**
    * Handle shots of the AI players.
    *
    * Generate random shoots for the medium AI player
    * @param r : seed to generate random coordinates
    * @param ships : List of ships of the other player
    * @param shipGrid : Grid which contain information about the placement of each ship
    * @return The result of the shoot (codeColor), the ship which have been touch and the coordinate of the shoot
    */
  def shotAIMedium(r : scala.util.Random, ships : List[Ship], shipGrid : Grid): (Int,Ship,Int,Int) ={
    val shotRow = r.nextInt(10)
    val shotCol = r.nextInt(10)
    if(shipGrid.gridFilled(shotRow)(shotCol)==1||shipGrid.gridFilled(shotRow)(shotCol)==2){
      shotAIMedium(r,ships,shipGrid)
    }else{
      val result = Player.makeShot(shotRow,shotCol,ships,shipGrid)
      (result._1,result._2,shotRow,shotCol)
    }
  }

  /**
    * Handle shots of the AI players.
    *
    * Generate random shoots for the hard AI player
    * @param r : seed to generate random coordinates
    * @param ships : List of ships of the other player
    * @param shipGrid : Grid which contain information about the placement of each ship
    * @param coordShipTouched : keep in memory the previous touched to shoot next to this position
    * @param dirShoot : Put a direction to maximise the chance to touch an other ship
    * @return The result of the shoot (codeColor), the ship which have been touch and the coordinate of the shoot
    */
  def shotAIHard(r : scala.util.Random, ships : List[Ship], shipGrid : Grid, coordShipTouched : List[Int], dirShoot : String): (Int,Ship,Int,Int) ={
    def shoot(shotRow : Int, shotCol : Int): (Int,Ship,Int,Int) ={
      if(shipGrid.gridFilled(shotRow)(shotCol)==1||shipGrid.gridFilled(shotRow)(shotCol)==2){
        shotAIHard(r,ships,shipGrid,List(-1,-1),dirShoot)
      }else{
        val result = Player.makeShot(shotRow,shotCol,ships,shipGrid)
        (result._1,result._2,shotRow,shotCol)
      }
    }
    if(coordShipTouched.head == -1){
      shoot(r.nextInt(10),r.nextInt(10))
    }else{
      dirShoot match {
        case "right" => if(coordShipTouched(1)<9){shoot(coordShipTouched(0),coordShipTouched(1)+1)}else{shotAIHard(r,ships,shipGrid,List(-1,-1),dirShoot)}
        case "left" => if(coordShipTouched(1)>0){shoot(coordShipTouched(0),coordShipTouched(1)-1)}else{shotAIHard(r,ships,shipGrid,List(-1,-1),dirShoot)}
        case "top" => if(coordShipTouched(0)<9){shoot(coordShipTouched(0)+1,coordShipTouched(1))}else{shotAIHard(r,ships,shipGrid,List(-1,-1),dirShoot)}
        case "bottom" =>  if(coordShipTouched(0)>0){shoot(coordShipTouched(0)-1,coordShipTouched(1))}else{shotAIHard(r,ships,shipGrid,List(-1,-1),dirShoot)}
      }
    }

  }

  /**
    * Create games between the AI players.
    *
    * Create 3 games test the AI players, each match will contain 100 games.
    */
  def aIPlay() : Unit = {
    val easy = Player("easy",shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()), isAI = true)
    val medium = Player("medium",shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()), isAI = true)
    val hard = Player("hard",shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()), isAI = true)
    var games : List[Game] = List(Game(players = List(easy, medium)),Game(players = List(easy, hard)),Game(players = List(medium, hard)))
    games = games.map(g => {
      Game.play(g,100)
    })
    games.foreach(g => println(g.players(0).name + " : " + g.players(0).score + " || " + g.players(1).name + " : " + g.players(1).score))

    val result = games.map(g => g.players(0).name + " ; " + g.players(0).score + " ; " + g.players(1).name + " ; " + g.players(1).score)
    val outputFile = new BufferedWriter(new FileWriter("ai_proof.csv"))
    outputFile.write(
      "AI Name;score;AI Name2;score2\n" + result(0) + "\n" +result(1) +"\n" +result(2)
    )
    outputFile.close()
  }
}