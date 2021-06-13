package modules

import enumeration.Roles
import model.{Character, Score}

import scala.annotation.switch
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.util.control.Breaks.{break, breakable}

class Game(val _scoreboard: Scoreboard) {
  var boatAtRiverBankA: Boolean = false
  var riverBankA: List[Character] = List[Character]()
  var riverBankB: List[Character] = List[Character]()
  var startTime: Long = 0
  var steps: Int = 0

  def start(_trialCount: Int): Unit = {
    initializeVariables()
    breakable {
      while (true) {
        steps += 1
        displayDistribution()
        val selectionList: List[Int] = promptInput()
        if (validateSelection(selectionList)) {
          val (tempRiverBankA, tempRiverBankB) = moveSelectedCharacterByIndex(selectionList)
          if (validateRiverBank(tempRiverBankA) && validateRiverBank(tempRiverBankB)) {
            riverBankA = tempRiverBankA
            riverBankB = tempRiverBankB
            boatAtRiverBankA = !boatAtRiverBankA
            if (riverBankA.length == 0) {
              println("Congratulations, everyone crossed the river safe and sound")
              break
            } else {
              println("Next Move")
            }
          } else {
            print("Please try again! ")
            readLine("Press enter to continue..")
          }
        } else {
          print("Please try again! ")
          readLine("Press enter to continue..")
        }
      }
    }
    val elapsedTime = (System.nanoTime - startTime) / 1e9d
    println(s"Time taken to complete the challenge: %02f seconds".format(elapsedTime))
    println(s"Steps taken: ${steps}")
    _scoreboard.save(new Score(
      trialCount = _trialCount,
      timeTaken = elapsedTime,
      moveCount = steps
    ))
  }

  def initializeVariables(): Unit = {
    boatAtRiverBankA = true
    riverBankA = List(
      new Character(role = Roles.Father),
      new Character(role = Roles.Mother),
      new Character(role = Roles.Son),
      new Character(role = Roles.Son),
      new Character(role = Roles.Daughter),
      new Character(role = Roles.Daughter),
      new Character(role = Roles.Criminal),
      new Character(role = Roles.Police),
    )
    riverBankB = List[Character]()
    startTime = System.nanoTime()
    steps = 0
  }

  def promptInput(): List[Int] = {
    if (boatAtRiverBankA) {
      println("Select up to 2 characters to board the boat to go to river bank B")
    } else {
      println("Select up to 2 characters to return from river bank B to A")
    }
    println("Enter index to select or \'N/n\' to stop selection")
    val selectionList: ListBuffer[Int] = ListBuffer[Int]()
    for (i <- 0 to 1) {
      breakable {
        while (true) {
          val index = readLine(s"Select ${if (i == 0) "1st" else "2nd"} character\n")
          if (!index.isEmpty) {
            if (index.toLowerCase() == "n") {
              if (selectionList.length == 0) {
                println("Please select at least 1 character to board the boat")
              } else {
                return selectionList.toList
              }
            } else {
              try {
                var value = index.toInt - 1
                if (!selectionList.contains(value)) {

                  if (value < (if (boatAtRiverBankA) riverBankA.length else riverBankB.length)) {
                    selectionList += value
                    break
                  } else {
                    println("The index entered is invalid, please try again")
                  }
                } else {
                  println("You have already selected the character, please try again")
                }

              } catch {
                case e: Exception => {
                  println("The index entered is invalid, please try again")
                }
              }
            }
          } else {
            println("The selection index cannot be empty")

          }
        }
      }
    }
    selectionList.toList
  }

  def displayDistribution(): Unit = {
    printSpacer()
    println("Character at River Bank A: ")
    printSpacer()
    if (riverBankA.length == 0) println("No one is here yet!")
    else {
      for (i <- 0 to riverBankA.length - 1) {
        println(s"${i + 1}. ${riverBankA(i)}")
      }
    }
    printSpacer()
    println("Character at River Bank B:")
    printSpacer()
    if (riverBankB.length == 0) println("No one is here yet!")
    else {
      for (i <- 0 to riverBankB.length - 1) {
        println(s"${i + 1}. ${riverBankB(i)}")
      }
    }
    printSpacer()
  }

  def moveSelectedCharacterByIndex(selectionList: List[Int]): (List[Character], List[Character]) = {
    var characterListA: List[Character] = null
    var characterListB: List[Character] = null

    if (boatAtRiverBankA) {
      characterListA = for {
        (x, i) <- riverBankA.zipWithIndex
        if !selectionList.contains(i)
      } yield x
      characterListB = selectionList.map(index => riverBankA(index)) ++ riverBankB
    } else {
      characterListA = selectionList.map(index => riverBankB(index)) ++ riverBankA
      characterListB = for {
        (x, i) <- riverBankB.zipWithIndex
        if !selectionList.contains(i)
      } yield x
    }
    (characterListA, characterListB)
  }

  def printSpacer(): Unit = println("=" * 50)

  def validateSelection(selectionList: List[Int]): Boolean = {
    val selectedCharacter: List[Character] = selectionList.map(
      index => if (boatAtRiverBankA) riverBankA(index) else riverBankB(index)
    )
    if (selectedCharacter.find(character => character.isAdult()) == None) {
      if (selectedCharacter.find(character => character.isCriminal()) != None) {
        println("Criminal is not allowed to operate the boat")
      } else {
        println("Only adult can operate the boat, please select at least an adult")
      }
      return false
    }
    true
  }

  def validateRiverBank(characterList: List[Character]): Boolean = {
    (!dadIsWithDaughterWithoutMum(characterList) &&
      !momIsWithBoyWithoutDad(characterList) &&
      !criminalWithFamilyWithoutPolice(characterList))
  }

  def dadIsWithDaughterWithoutMum(characterList: List[Character]): Boolean = {
    var hasDad: Boolean = false
    var hasDaughter: Boolean = false
    var withoutMum: Boolean = true
    characterList.foreach(candidate => (candidate.role: @switch) match {
      case Roles.Father => if (!hasDad) hasDad = true
      case Roles.Daughter => if (!hasDaughter) hasDaughter = true
      case Roles.Mother => if (withoutMum) withoutMum = false
      case _ =>
    })
    if (hasDad && hasDaughter && withoutMum) {
      println("Dad cannot be in the presence of the girls without Mom.")
      return true
    }
    false
  }

  def momIsWithBoyWithoutDad(characterList: List[Character]): Boolean = {
    var hasMum: Boolean = false
    var hasBoy: Boolean = false
    var withoutDad: Boolean = true
    characterList.foreach(candidate => (candidate.role: @switch) match {
      case Roles.Mother => if (!hasMum) hasMum = true
      case Roles.Son => if (!hasBoy) hasBoy = true
      case Roles.Father => if (withoutDad) withoutDad = false
      case _ =>
    })
    if (hasMum && hasBoy && withoutDad) {
      println("Mom cannot be in the presence of the boys without Dad.")
      return true
    }
    false
  }

  def criminalWithFamilyWithoutPolice(characterList: List[Character]): Boolean = {
    var hasCriminal: Boolean = false
    var hasFamily: Boolean = false
    var withoutPolice: Boolean = true
    characterList.foreach(candidate => (candidate.role: @switch) match {
      case Roles.Criminal => if (!hasCriminal) hasCriminal = true
      case Roles.Police => if (withoutPolice) withoutPolice = false
      case _ => if (!hasFamily) hasFamily = true
    })
    if (hasCriminal && hasFamily && withoutPolice) {
      println("Criminal is alone with the family")
      return true
    }
    false
  }
}