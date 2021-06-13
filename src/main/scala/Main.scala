import modules.{Game, Scoreboard}

import scala.io.StdIn.readLine
import scala.util.control.Breaks.{break, breakable}

object Main {
  def main(args: Array[String]): Unit = {
    val scoreboard: Scoreboard = new Scoreboard()
    val game: Game = new Game(_scoreboard = scoreboard);
    println("Welcome")
    var trialCount = 1
    breakable {
      while (true) {
        println(s"Trial #${trialCount}")
        println("Please select an action to continue")
        println("1. Start Game")
        println("2. View Scoreboard")
        breakable {
          while (true) {
            val selection = readLine("Selection: ")
            if (!selection.isEmpty) {
              try {
                var value = selection.toInt
                if (value == 1) {
                  game.start(_trialCount = trialCount)
                  trialCount += 1
                  break
                } else if (value == 2) {
                  scoreboard.display()
                  break
                }
                else {
                  println("The index entered is invalid, please try again")
                }
              } catch {
                case e: Exception => {
                  println("The index entered is invalid, please try again")
                }
              }
            }
          }
        }
        println("Wish to continue?")
        val index = readLine("Press Y/y to continue or other keys to quit...\n")
        if (index.toLowerCase != "y") {
          break
        }
      }
    }
  }
}
