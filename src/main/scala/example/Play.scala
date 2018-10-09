package example

object Play extends App {
  println("Enter the mode of play :")
  println("[1 for player against player]")
  println("[2 for player against AI]")
  println("[3 to test the AI performance]")
  var players = readLine("Enter the mode of play : ")


  players match {
      //TD create a function to hide duplicated code
    case "1" =>
      val game = Game(players = List(Game.createPlayer(false), Game.createPlayer(false)))
      val result = Game.play(game,1)
      println(result.players(0).name + " : " + result.players(0).score + " || " + result.players(1).name + " : " + result.players(1).score)
    case "2" =>
      val game = Game(players = List(Game.createPlayer(false), Game.createPlayer(true)))
      val result = Game.play(game,1)
      println(result.players(0).name + " : " + result.players(0).score + " || " + result.players(1).name + " : " + result.players(1).score)
    case "3" => ArtificialPlayer.aIPlay()
    case _ => println("<!> Input didn't recognize")
  }
}