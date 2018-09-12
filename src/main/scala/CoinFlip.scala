import CoinFlipUtils._

import scala.util.Random

case class GameState(numFlips: Int, numCorrect: Int)

object CoinFlip extends App {
  print(Console.BLUE_B)
  print("_______________________________")
  print(Console.RESET)
  println()
  print(Console.RED)
  print(Console.BOLD)
  print("         - Welcome -           ")
  print(Console.RESET)
  println()
  homeScreen()
  println()
  print(Console.RED_B)
  print("_______________________________")
  print(Console.RESET)
  selectScreen()

  def mainLoop(gameState: GameState, random: Random, gameSelect: Int) {

    val userInput = BestOf10(gameSelect, gameState)

    // handle the result
    userInput match {
      case "H" | "T" => {
        val coinTossResult = tossCoin(random)
        val newNumFlips = gameState.numFlips + 1
        if (userInput == coinTossResult) {
          val newNumCorrect = gameState.numCorrect + 1
          val newGameState = gameState.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
          printGameState(printableFlipResult(coinTossResult), newGameState, "Awesome")
          mainLoop(newGameState, random, gameSelect)
        } else {
          val newGameState = gameState.copy(numFlips = newNumFlips)
          printGameState(printableFlipResult(coinTossResult), newGameState, "You Suck")
          mainLoop(newGameState, random, gameSelect)
        }
      }
      case "Q" => {
        if (gameSelect == 1 && gameState.numFlips == 10) println("\n\nGoodBye!")
        else {
          printGameOver()
          printGameState(gameState)
          winPercentage(gameState)
          println("\nGoodBye!")
          // return out of the recursion here
        }
      }

      case "M" => {
        if (gameSelect == 1 && gameState.numFlips == 10) {
          println("\n_______________________________")
          println("\n")
          homeScreen()
          selectScreen()
        }
        else {
          printGameOver()
          printGameState(gameState)
          winPercentage(gameState)
          println("_______________________________")
          println("\n")
          homeScreen()
          selectScreen()
        }
      }

      case _ ⇒ {
        val newNumFlips = gameState.numFlips
        val newNumCorrect = gameState.numCorrect
        println()
        print(Console.BOLD)
        print(Console.RED)
        print(Console.REVERSED)
        print("Incorrect input. Please try again")
        print(Console.RESET)
        println()
        printGameState(gameState)
        println("_______________________________")
        val newGameState = gameState.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
        mainLoop(newGameState, random, gameSelect)
      }
    }
  }
}