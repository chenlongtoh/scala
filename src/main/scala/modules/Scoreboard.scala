package modules

import model.Score

import scala.collection.mutable.ListBuffer

class Scoreboard {
  var scoreList: ListBuffer[Score] = ListBuffer[Score]()

  val trialColumnSpacing = 10
  val moveCountColumnSpacing = 20
  val timeTakenColumnSpacing = 20
  val smallestStepsColumnSpacing = 20
  val fastestTimeColumnSpacing = 10
  val totalSpacing = trialColumnSpacing + moveCountColumnSpacing + timeTakenColumnSpacing + smallestStepsColumnSpacing + fastestTimeColumnSpacing

  def save(score: Score): Unit = scoreList += score

  def printSpacer(): Unit = println("=" * totalSpacing)

  def printResult(): Unit = {
    if (scoreList.isEmpty) {
      println("The scoreboard is empty, start playing to see your scores")
    } else {
      val tick = "\u2713"
      val fastestTime = scoreList.toList.map(_.timeTaken).min
      val smallestMoves = scoreList.toList.map(_.moveCount).min
      scoreList.foreach((score) => {
        println(
          s"%${trialColumnSpacing}s%${moveCountColumnSpacing}s%${timeTakenColumnSpacing}s%${smallestStepsColumnSpacing}s%${fastestTimeColumnSpacing}s"
            .format(score.trialCount, score.moveCount, score.timeTaken,
              if (score.moveCount == smallestMoves) tick else "",
              if (score.timeTaken == fastestTime) tick else "",
            )
        )
      })
    }
  }

  def display(): Unit = {
    printSpacer()
    println(" " * (((totalSpacing - "Scoreboard".length) / 2).floor.toInt) + "Scoreboard")
    printSpacer()
    println(s"%${trialColumnSpacing}s%${moveCountColumnSpacing}s%${timeTakenColumnSpacing}s%${smallestStepsColumnSpacing}s%${fastestTimeColumnSpacing}s"
      .format("#Trial", "Moves Taken", "Time Taken (s)", "Least Moves", "Fastest"))
    printSpacer()
    printResult()
    printSpacer()
  }
}
