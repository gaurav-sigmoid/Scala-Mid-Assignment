import scala.collection.mutable.ArrayBuffer

class playerStats(val year: Int, val playerName: String, val country: String, val matches: Int, val runs: Int, val Wickets: Int){

  val overall_score = (this.Wickets * 5) + (this.runs * 0.05)

  def highest_score(players: ArrayBuffer[playerStats]): playerStats ={
    var temp = 0
    var count = 0
    while (count<players.size){
      temp = if(players(count).runs > temp) players(count).runs else temp
      count += 1
    }

    count = 0
    var obj: playerStats = players(0)
    while(count<players.size){
      obj = if(players(count).runs == temp) players(count) else obj
      count += 1
    }
    return obj
  }
  def most_wickets(players: ArrayBuffer[playerStats]): playerStats ={
    var temp = 0
    var count = 0
    while (count<players.size){
      temp = if(players(count).Wickets > temp) players(count).Wickets else temp
      count += 1
    }

    count = 0
    var obj: playerStats = players(0)
    while(count<players.size){
      obj = if(players(count).Wickets == temp) players(count) else obj
      count += 1
    }
    return obj
  }

  def best_overall(players: ArrayBuffer[playerStats]): playerStats ={
    var temp = 0.0
    var count = 0
    while (count<players.size){
      temp = if(players(count).overall_score > temp) players(count).overall_score else temp
      count += 1
    }

    count = 0
    var obj: playerStats = players(0)
    while(count<players.size){
      obj = if(players(count).overall_score == temp) players(count) else obj
      count += 1
    }
    return obj
  }

  def top5_runs(players: ArrayBuffer[playerStats]): Unit ={
    val newPlayers = players map (identity)
    var count = 0
    while(count<5) {
      val temp = this.highest_score(newPlayers)
      this.printPlayer(temp)
      newPlayers -= (temp)
      count += 1
    }
  }

  def top5_wickets(players: ArrayBuffer[playerStats]): Unit ={
    val newPlayers = players map (identity)
    var count = 0
    while(count<5) {
      val temp = this.most_wickets(newPlayers)
      this.printPlayer(temp)
      newPlayers -= (temp)
      count += 1
    }
  }

  def overall_perf_ranked(players: ArrayBuffer[playerStats]): Unit ={
    val newPlayers = players map (identity)
    var count = 0
    val len = players.size
    while(count<len) {
      val temp = this.best_overall(newPlayers)
      this.printPlayer(temp)
      newPlayers -= (temp)
      count += 1
    }
  }
  def printPlayer(player: playerStats): Unit ={
    println(s"${player.year} ${player.playerName} ${player.country} ${player.matches} ${player.runs} ${player.Wickets}")
  }

}

object Question2 extends App {
  val p1 = new playerStats(2021, playerName = "Sam", "India", 23, 2300, 3)
  val p2 = new playerStats(2021, playerName = "Ram", "India", 23, 300, 30)
  val p3 = new playerStats(2021, playerName = "Mano", "India", 23, 300, 13)
  val p4 = new playerStats(2021, playerName = "Sumo", "India", 23, 245, 32)
  val p5 = new  playerStats(2021, playerName = "Nero", "India", 23, 1284, 5)
  val p6 = new playerStats(2021, playerName = "Max", "India", 23, 105, 42)
  val p7 = new playerStats(2021, playerName = "Adam", "India", 23, 2314, 1)
  val p8 = new playerStats(2021, playerName = "Wind", "India", 23, 1812, 28)
  val p9 = new playerStats(2021, playerName = "Alex", "India", 23, 516, 14)
  val p10 = new playerStats(2021, playerName = "Grey", "India", 23, 315, 19)

  val players = ArrayBuffer[playerStats]()
  players += (p1)
  players += (p2)
  players += (p3)
  players += (p4)
  players += (p5)
  players += (p6)
  players += (p7)
  players += (p8)
  players += (p9)
  players += (p10)
  println("Player with the highest run scored: ")
  p1.printPlayer(p1.highest_score(players))
  println()
  println("Top 5 players by run scored: ")
  p1.top5_runs(players)
  println()
  println("Top 5 players by wicket taken")
  p1.top5_wickets(players)
  println()
  println("Rank players with overall performance give weight 5x to wicket taken and (5/100)x to run scored")
  p1.overall_perf_ranked(players)
  println()
}
