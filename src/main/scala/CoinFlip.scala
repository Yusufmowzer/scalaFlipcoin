import CoinFlipUtils._
import scala.annotation.tailrec
import scala.util.Random

case class GameState(numFlips: Int, numCorrect: Int)

object CoinFlip extends App {

  val r = Random
  val s = GameState(0, 0)
  mainLoop(s, r)

  @tailrec
  def mainLoop(gameState: GameState, random: Random) {

    showPrompt()
    val userInput = getUserInput()

    // handle the result
    userInput match {
      case "H" | "T" => {
        val coinTossResult = tossCoin(random)
        val newNumFlips = gameState.numFlips + 1
        if (userInput == coinTossResult) {
          val newNumCorrect = gameState.numCorrect + 1
          val newGameState = gameState.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
          printGameState(printableFlipResult(coinTossResult), newGameState, "Awesome")
          mainLoop(newGameState, random)
        } else {
          val newGameState = gameState.copy(numFlips = newNumFlips)
          printGameState(printableFlipResult(coinTossResult), newGameState, "You Suck")
          mainLoop(newGameState, random)
        }
      }
      case "Q" => {
        printGameOver()
        printGameState(gameState)
        winPercentage(gameState)
        // return out of the recursion here
      }
      case _ ⇒ {
        val newNumFlips = gameState.numFlips
        val newNumCorrect = gameState.numCorrect
        println("Incorrect input. Please try again")
        printGameState(gameState)
        val newGameState = gameState.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
        mainLoop(newGameState, random)
      }
    }
  }
}
