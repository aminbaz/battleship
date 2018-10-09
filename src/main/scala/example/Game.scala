package example

import scala.annotation.tailrec

case class Game(players : List[Player]) {

}
object Game{
  /**
    * Launch a game.
    *
    * Launch the workflow of the game
    * @param gamePlay : The initialized game
    * @param numberOfGame : the number of iteration of the game
    * @return the updated game with scores
    */
  @tailrec
  def play(gamePlay : Game, numberOfGame : Int) : Game = {
    if(numberOfGame == 0){
      gamePlay
    }else{
      val r = new scala.util.Random()
      val nameShips = List("Carrier", "Battleship", "Cruiser", "Submarine","Destroyer")
      var player2 = gamePlay.players(1)
      var player1 = gamePlay.players(0)

      if(player1.isAI) {
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(ships = ArtificialPlayer.createShips(r,nameShips, List())))
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(gridFilled = Grid.placeShip(player1.shipGrid.ships,player1.shipGrid.gridFilled)))
      }else{
        println(">>>>> " + player1.name + " : Placement of ships")
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(ships = player1.shipGrid.createShips(nameShips, List())))
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(gridFilled = Grid.placeShip(player1.shipGrid.ships, player1.shipGrid.gridFilled)))

      }
      if(player2.isAI){
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(ships = ArtificialPlayer.createShips(r,nameShips, List())))
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(gridFilled = Grid.placeShip(player2.shipGrid.ships,player2.shipGrid.gridFilled)))

      }else{
        println(">>>>> " + player2.name + " : Placement of ships")
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(ships = player2.shipGrid.createShips(nameShips, List())))
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(gridFilled = Grid.placeShip(player2.shipGrid.ships,player2.shipGrid.gridFilled)))

      }
      val players = turnOfPlay(player1,player2,r)
      val resultGame = gamePlay.copy(players=List(players(0).copy(shipGrid = Grid(List()),shotGrid = Grid(List())),players(1).copy(score = players(1).score+1,shipGrid = Grid(List()),shotGrid = Grid(List()))))

      if(player1.isAI&&player2.isAI){
        play(resultGame, numberOfGame-1)
      }else{
        if(readLine("Replay ? (y for YES and n for NO : ")=="y"){
          play(resultGame, 1)
        }else{
          resultGame
        }
      }
    }
  }

  /**
    * Player's movement.
    *
    * Record of the action of the attack player
    * @param player1 : Attack player
    * @param player2 : Defence player
    * @param r : Seed to generate random coordinates
    * @return The updated players by the attack player's movement
    */
  def turnOfPlay(player1 : Player, player2 : Player,  r : scala.util.Random) : List[Player] = {
    def checkInputShot(input : String): Option[Int] ={
      try {
        if(input.toInt<1||input.toInt>10){
          println("<!> Your value is out of bound, re-enter a value between 1 and 10")
          None
        }else{Some(input.toInt -1)}
      } catch {
        case _: Exception =>
          println("<!> Your value is not an integer")
          None
      }
    }

    def checkEndGame(grid : List[List[Int]]): Boolean={
      val gridToView = grid.flatten
      if(gridToView contains 3){
        false
      }else{
        true
      }
    }

    if(checkEndGame(player1.shipGrid.gridFilled)){
      if(!player1.isAI) {println("End of the game, the winner is : "+ player2.name)}
      List(player1,player2)
    }else{
      var playerAttack = player1
      var playerDefense = player2
      var shotRow : Int = 0
      var shotCol : Int = 0
      val resultShot : (Int,Ship,Int,Int) = {
        if(playerAttack.isAI){
          if(playerAttack.dirShootAI.isEmpty){
            playerAttack = playerAttack.copy(coordShipTouchedAI = List(-1,-1),dirShootAI = List("top","right","left","bottom"))
          }
          var shotAI : (Int,Ship,Int,Int) = (0,playerDefense.shipGrid.ships.head,0,0)
          playerAttack.name match {
            case "easy" => shotAI = ArtificialPlayer.shotAIEasy(r,playerDefense.shipGrid.ships,playerDefense.shipGrid)
            case "medium" => shotAI = ArtificialPlayer.shotAIMedium(r,playerDefense.shipGrid.ships,playerDefense.shipGrid)
            case "hard" => {
              shotAI = ArtificialPlayer.shotAIHard(r,playerDefense.shipGrid.ships,playerDefense.shipGrid,playerAttack.coordShipTouchedAI,playerAttack.dirShootAI.head)
              if(shotAI._1==2){
                playerAttack = playerAttack.copy(coordShipTouchedAI = List(shotAI._3,shotAI._4),dirShootAI = List("top","right","left","bottom"))
              }else if(shotAI._1==1&&playerAttack.coordShipTouchedAI.head!= -1){
                playerAttack = playerAttack.copy(dirShootAI = playerAttack.dirShootAI.tail)
              }else{
                playerAttack = playerAttack.copy(coordShipTouchedAI = List(-1,-1), dirShootAI = List("top","right","left","bottom"))
              }
            }
          }
          shotRow = shotAI._3
          shotCol = shotAI._4
          shotAI
        }else{
          println(">>>>>>>>>> Turn of " + player1.name + "!")
          println(">>>>>>>>>> Grid of my ships : ")
          Grid.printGrid(playerAttack.shipGrid.gridFilled)
          println(">>>>>>>>>> Grid of my shots : ")
          Grid.printGrid(playerAttack.shotGrid.gridFilled)
          val shotRowOption = checkInputShot(readLine("Shoot row (value between 1 and 10) : "))
          val shotColOption = checkInputShot(readLine("Shoot col (value between 1 and 10) : "))
          if(shotRowOption.isEmpty||shotColOption.isEmpty){
            turnOfPlay(playerAttack,playerDefense,r)
          }else{
            shotRow = shotRowOption.get
            shotCol = shotColOption.get
          }
          val playerShoot = Player.makeShot(shotRow,shotCol,playerDefense.shipGrid.ships,playerDefense.shipGrid)
          (playerShoot._1,playerShoot._2,0,0)
        }
      }
      if(resultShot._1 == 2) {
        val indexShip = playerDefense.shipGrid.ships.indexOf(resultShot._2)
        val shipUpdated = playerDefense.shipGrid.ships(indexShip).copy(lifePoint =  playerDefense.shipGrid.ships(indexShip).lifePoint -1)
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(ships =  playerDefense.shipGrid.ships.updated(indexShip,shipUpdated)))
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        if(shipUpdated.lifePoint == 0 && !playerAttack.isAI && !playerDefense.isAI) {
          println("You sank my " + shipUpdated.typeShip + " !")
        }
        turnOfPlay(playerDefense,playerAttack,r)
      }else if(resultShot._1 == -1){
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(1, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(1, List(shotRow, shotCol))))
        turnOfPlay(playerDefense,playerAttack,r)
      }else if(resultShot._1 == -2){
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(2, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(2, List(shotRow, shotCol))))
        turnOfPlay(playerDefense,playerAttack,r)
      }else{
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        turnOfPlay(playerDefense,playerAttack,r)
      }
    }
  }

  /**
    * Create a player.
    *
    * Init a player
    * @param aiPlayer : boolean to create a real of artificial player
    * @return The player created.
    */
  @tailrec
  def createPlayer(aiPlayer:Boolean) : Player = {
    if(aiPlayer){
      val level = readLine("Enter the level of the AI (easy, medium or hard) => ")
      if(level == "easy" || level == "medium"||level == "hard"){
        Player(name = level, shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()), isAI = true)
      }else{
        println("<!> Wrong input, choose between easy, medium or hard")
        createPlayer(true)
      }
    }else{
      val namePlayer = readLine("Player name => ")
      Player(name = namePlayer, shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()))
    }
  }
}